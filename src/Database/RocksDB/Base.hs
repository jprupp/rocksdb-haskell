{-# LANGUAGE LambdaCase      #-}
-- |
-- Module      : Database.RocksDB.Base
-- Copyright   : (c) 2012-2013 The leveldb-haskell Authors
--               (c) 2014-2020 The rocksdb-haskell Authors
-- License     : BSD3
-- Maintainer  : jprupp@protonmail.ch
-- Stability   : experimental
-- Portability : non-portable
--
-- RocksDB Haskell binding.
--
-- The API closely follows the C-API of RocksDB.

module Database.RocksDB.Base
    ( -- * Exported Types
      DB (..)
    , BatchOp (..)
    , Range

    -- * Options
    , Config (..)

    -- * Basic Database Manipulations
    , withDB
    , put
    , delete
    , write
    , get
    , withSnapshot

    -- * Filter Policy / Bloom Filter
    , BloomFilter
    , withBloomFilter

    -- * Administrative Functions
    , Property (..), getProperty
    , destroy
    , repair
    , approximateSize

    -- * Iteration
    , module Database.RocksDB.Iterator
    ) where

import           Control.Monad             ((>=>))
import           Data.ByteString           (ByteString)
import qualified Data.ByteString           as BS
import           Data.ByteString.Internal  (ByteString (..))
import           Database.RocksDB.C
import           Database.RocksDB.Internal
import           Database.RocksDB.Iterator
import           Foreign
import           Foreign.C.String          (CString, withCString)
import qualified GHC.Foreign               as GHC
import qualified GHC.IO.Encoding           as GHC
import           System.Directory          (createDirectoryIfMissing)
import           UnliftIO

-- | Properties exposed by RocksDB
data Property = NumFilesAtLevel Int | Stats | SSTables
    deriving (Eq, Show)

data BatchOp = Put !ByteString !ByteString
             | Del !ByteString
             | PutCF !ColumnFamily !ByteString !ByteString
             | DelCF !ColumnFamily !ByteString
             deriving (Eq, Show)

-- | Create a 'BloomFilter'
withBloomFilter :: MonadUnliftIO m => Int -> (BloomFilter -> m a) -> m a
withBloomFilter i =
    bracket create_bloom_filter destroy_bloom_filter
  where
    destroy_bloom_filter = liftIO . c_rocksdb_filterpolicy_destroy
    create_bloom_filter = liftIO $
        c_rocksdb_filterpolicy_create_bloom (intToCInt i)

-- | Open a database.
--
-- The returned handle should be released with 'close'.
withDB :: MonadUnliftIO m => FilePath -> Config -> (DB -> m a) -> m a
withDB path config f =
    withOptions config $ \opts_ptr ->
    withReadOpts Nothing $ \read_opts ->
    withWriteOpts $ \write_opts ->
    bracket (create_db opts_ptr read_opts write_opts) destroy_db f
  where
    destroy_db db = liftIO $
        c_rocksdb_close $ rocksDB db
    create_db opts_ptr read_opts write_opts = liftIO $ do
        createDirectoryIfMissing True path
        withFilePath path $ \path_ptr -> do
            db_ptr <- throwIfErr "open" $
                c_rocksdb_open opts_ptr path_ptr
            return $ DB db_ptr [] read_opts write_opts

-- | Run an action with a 'Snapshot' of the database.
withSnapshot :: MonadUnliftIO m => DB -> (DB -> m a) -> m a
withSnapshot db@DB{rocksDB = db_ptr} f =
    bracket create_snapshot release_snapshot (f . fst)
  where
    release_snapshot = liftIO . c_rocksdb_release_snapshot db_ptr . snd
    create_snapshot = liftIO $ do
        snap_ptr <- c_rocksdb_create_snapshot db_ptr
        withReadOpts (Just snap_ptr) $ \read_opts ->
            return (db{readOpts = read_opts}, snap_ptr)

-- | Get a DB property.
getProperty :: MonadIO m => DB -> Property -> m (Maybe ByteString)
getProperty DB{rocksDB = db_ptr} p = liftIO $
    withCString (prop p) $
    c_rocksdb_property_value db_ptr >=> \case
    val_ptr | val_ptr == nullPtr -> return Nothing
            | otherwise -> do
                  res <- Just <$> BS.packCString val_ptr
                  freeCString val_ptr
                  return res
  where
    prop (NumFilesAtLevel i) = "rocksdb.num-files-at-level" ++ show i
    prop Stats               = "rocksdb.stats"
    prop SSTables            = "rocksdb.sstables"

-- | Destroy the given RocksDB database.
destroy :: MonadIO m => FilePath -> Options -> m ()
destroy path opts_ptr = liftIO $
    withFilePath path $ \path_ptr ->
    throwIfErr "destroy" $ c_rocksdb_destroy_db opts_ptr path_ptr

-- | Repair the given RocksDB database.
repair :: MonadIO m => FilePath -> Options -> m ()
repair path opts_ptr = liftIO $
    withFilePath path $ \path_ptr ->
    throwIfErr "repair" $ c_rocksdb_repair_db opts_ptr path_ptr


-- TODO: support [Range], like C API does
type Range  = (ByteString, ByteString)

-- | Inspect the approximate sizes of the different levels.
approximateSize :: MonadIO m => DB -> Range -> m Int64
approximateSize DB{rocksDB = db_ptr} (from, to) = liftIO $
    BS.useAsCStringLen from $ \(from_ptr, flen) ->
    BS.useAsCStringLen to   $ \(to_ptr, tlen)   ->
    withArray [from_ptr]          $ \from_ptrs        ->
    withArray [intToCSize flen]   $ \flen_ptrs        ->
    withArray [to_ptr]            $ \to_ptrs          ->
    withArray [intToCSize tlen]   $ \tlen_ptrs        ->
    allocaArray 1                 $ \size_ptrs        -> do
        c_rocksdb_approximate_sizes db_ptr 1
                                    from_ptrs flen_ptrs
                                    to_ptrs tlen_ptrs
                                    size_ptrs
        fmap head $ peekArray 1 size_ptrs >>= mapM toInt64

    where
        toInt64 = return . fromIntegral

-- | Write a key/value pair.
put :: MonadIO m => DB -> ByteString -> ByteString -> m ()
put DB{rocksDB = db_ptr, writeOpts = write_opts} key value = liftIO $
    BS.useAsCStringLen key   $ \(key_ptr, klen) ->
    BS.useAsCStringLen value $ \(val_ptr, vlen) ->
        throwIfErr "put"
            $ c_rocksdb_put db_ptr write_opts
                            key_ptr (intToCSize klen)
                            val_ptr (intToCSize vlen)

-- | Read a value by key.
get :: MonadIO m => DB -> ByteString -> m (Maybe ByteString)
get DB{rocksDB = db_ptr, readOpts = read_opts} key = liftIO $
    BS.useAsCStringLen key $ \(key_ptr, klen) ->
    alloca                 $ \vlen_ptr -> do
        val_ptr <- throwIfErr "get" $
            c_rocksdb_get db_ptr read_opts key_ptr (intToCSize klen) vlen_ptr
        vlen <- peek vlen_ptr
        if val_ptr == nullPtr
            then return Nothing
            else do
                res' <- Just <$> BS.packCStringLen (val_ptr, cSizeToInt vlen)
                freeCString val_ptr
                return res'

-- | Delete a key/value pair.
delete :: MonadIO m => DB -> ByteString -> m ()
delete DB{rocksDB = db_ptr, writeOpts = write_opts} key = liftIO $
    BS.useAsCStringLen key $ \(key_ptr, klen) ->
        throwIfErr "delete"
            $ c_rocksdb_delete db_ptr write_opts key_ptr (intToCSize klen)

-- | Perform a batch mutation.
write :: MonadIO m => DB -> [BatchOp] -> m ()
write DB{rocksDB = db_ptr, writeOpts = write_opts} batch = liftIO $
    bracket
    c_rocksdb_writebatch_create
    c_rocksdb_writebatch_destroy $ \batch_ptr -> do

    mapM_ (batchAdd batch_ptr) batch

    throwIfErr "write" $ c_rocksdb_write db_ptr write_opts batch_ptr

    -- ensure @ByteString@s (and respective shared @CStringLen@s) aren't GC'ed
    -- until here
    mapM_ (liftIO . touch) batch

    where
        batchAdd batch_ptr (Put key val) =
            BS.useAsCStringLen key $ \(key_ptr, klen) ->
            BS.useAsCStringLen val $ \(val_ptr, vlen) ->
                c_rocksdb_writebatch_put
                batch_ptr
                key_ptr (intToCSize klen)
                val_ptr (intToCSize vlen)

        batchAdd batch_ptr (PutCF cf_ptr key val) =
            BS.useAsCStringLen key $ \(key_ptr, klen) ->
            BS.useAsCStringLen val $ \(val_ptr, vlen) ->
                c_rocksdb_writebatch_put_cf
                batch_ptr
                cf_ptr
                key_ptr (intToCSize klen)
                val_ptr (intToCSize vlen)

        batchAdd batch_ptr (Del key) =
            BS.useAsCStringLen key $ \(key_ptr, klen) ->
                c_rocksdb_writebatch_delete
                batch_ptr
                key_ptr (intToCSize klen)

        batchAdd batch_ptr (DelCF cf_ptr key) =
            BS.useAsCStringLen key $ \(key_ptr, klen) ->
                c_rocksdb_writebatch_delete_cf
                batch_ptr
                cf_ptr
                key_ptr (intToCSize klen)


        touch (Put (PS p _ _) (PS p' _ _)) = do
            touchForeignPtr p
            touchForeignPtr p'

        touch (PutCF _ (PS p _ _) (PS p' _ _)) = do
            touchForeignPtr p
            touchForeignPtr p'

        touch (Del (PS p _ _)) = touchForeignPtr p

        touch (DelCF _ (PS p _ _)) = touchForeignPtr p

-- | Marshal a 'FilePath' (Haskell string) into a `NUL` terminated C string using
-- temporary storage.
withFilePath :: FilePath -> (CString -> IO a) -> IO a
withFilePath = GHC.withCString GHC.utf8
