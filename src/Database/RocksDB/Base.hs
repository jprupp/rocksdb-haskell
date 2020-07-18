{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
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
      DB
    , BatchOp (..)
    , Snapshot
    , Range

    -- * Options
    , Config (..)
    , Options (..)
    , newOptions
    , defaultOptions
    , ReadOpts
    , newReadOpts
    , WriteOpts
    , writeOpts

    -- * Basic Database Manipulations
    , open
    , withRocksDB
    , put
    , delete
    , write
    , get
    , getReadOpts
    , createSnapshot
    , withSnapshot

    -- * Filter Policy / Bloom Filter
    , BloomFilter
    , createBloomFilter
    , bloomFilter

    -- * Administrative Functions
    , Property (..), getProperty
    , destroy
    , repair
    , approximateSize

    -- * Iteration
    , module Database.RocksDB.Iterator
    ) where

import           Control.Monad             (when, (>=>))
import           Data.ByteString           (ByteString)
import qualified Data.ByteString           as BS
import           Data.ByteString.Internal  (ByteString (..))
import qualified Data.ByteString.Unsafe    as BU
import           Data.Default
import           Database.RocksDB.C
import           Database.RocksDB.Internal
import           Database.RocksDB.Iterator
import           Foreign
import           Foreign.C.String          (CString, withCString)
import qualified Foreign.Concurrent        as F
import qualified GHC.Foreign               as GHC
import qualified GHC.IO.Encoding           as GHC
import           System.Directory          (createDirectoryIfMissing)
import           UnliftIO

type Snapshot = ForeignPtr LSnapshot
type BloomFilter = ForeignPtr LBloomFilter

-- | Properties exposed by RocksDB
data Property = NumFilesAtLevel Int | Stats | SSTables
    deriving (Eq, Show)

data BatchOp = Put !ByteString !ByteString
             | Del !ByteString
             | PutCF !ColumnFamily !ByteString !ByteString
             | DelCF !ColumnFamily !ByteString
             deriving (Eq, Show)

-- | Create a 'BloomFilter'
bloomFilter :: MonadIO m => Int -> m BloomFilter
bloomFilter i = liftIO $ do
    bloom_ptr <- c_rocksdb_filterpolicy_create_bloom (intToCInt i)
    newForeignPtr c_rocksdb_filterpolicy_destroy bloom_ptr

withRocksDB :: MonadUnliftIO m => FilePath -> Options -> (DB -> m a) -> m a
withRocksDB path opts = bracket (open path opts) close
  where
    close (DB db_fptr _ col_fams) = liftIO $ do
        mapM_ (finalizeForeignPtr . fst) col_fams
        finalizeForeignPtr db_fptr

-- | Open a database.
--
-- The returned handle should be released with 'close'.
open :: MonadIO m => FilePath -> Options -> m DB
open path opts@Options {..} = liftIO $ do
    when (createIfMissing config) $ createDirectoryIfMissing True path
    withFilePath path $ \path_ptr ->
        withForeignPtr options' $ \opts_ptr -> do
            db_ptr <- throwIfErr "open" $ c_rocksdb_open opts_ptr path_ptr
            db_fptr <- newForeignPtr c_rocksdb_close db_ptr
            return $ DB db_fptr opts []

-- | Run an action with a 'Snapshot' of the database.
withSnapshot :: MonadUnliftIO m => DB -> (Snapshot -> m a) -> m a
withSnapshot db =
    bracket (createSnapshot db) (liftIO . finalizeForeignPtr)

-- | Create a snapshot of the database.
--
-- The returned 'Snapshot' should be released with 'releaseSnapshot'.
createSnapshot :: MonadIO m => DB -> m Snapshot
createSnapshot DB {..} =
    liftIO $ do
        snap_ptr <- withForeignPtr rocksDB c_rocksdb_create_snapshot
        let fin = withForeignPtr rocksDB $ \db_ptr ->
                  c_rocksdb_release_snapshot db_ptr snap_ptr
        F.newForeignPtr snap_ptr fin

-- | Get a DB property.
getProperty :: MonadIO m => DB -> Property -> m (Maybe ByteString)
getProperty DB {..} p = liftIO $
    withForeignPtr rocksDB $ \db_ptr ->
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
destroy path Options {..} =
    liftIO $
    withFilePath path $ \path_ptr ->
    withForeignPtr options' $ \opts_ptr ->
    throwIfErr "destroy" $ c_rocksdb_destroy_db opts_ptr path_ptr

-- | Repair the given RocksDB database.
repair :: MonadIO m => FilePath -> Options -> m ()
repair path Options {..} = liftIO $
    withFilePath path $ \path_ptr ->
    withForeignPtr options' $ \opts_ptr ->
    throwIfErr "repair" $ c_rocksdb_repair_db opts_ptr path_ptr


-- TODO: support [Range], like C API does
type Range  = (ByteString, ByteString)

-- | Inspect the approximate sizes of the different levels.
approximateSize :: MonadIO m => DB -> Range -> m Int64
approximateSize DB {..} (from, to) = liftIO $
    withForeignPtr rocksDB $ \db_ptr ->
    BU.unsafeUseAsCStringLen from $ \(from_ptr, flen) ->
    BU.unsafeUseAsCStringLen to   $ \(to_ptr, tlen)   ->
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
put DB {..} key value = liftIO $
    withForeignPtr rocksDB $ \db_ptr ->
    BU.unsafeUseAsCStringLen key   $ \(key_ptr, klen) ->
    BU.unsafeUseAsCStringLen value $ \(val_ptr, vlen) ->
        throwIfErr "put"
            $ c_rocksdb_put db_ptr writeOpts
                            key_ptr (intToCSize klen)
                            val_ptr (intToCSize vlen)

get :: MonadIO m => DB -> ByteString -> m (Maybe ByteString)
get db = getReadOpts db def

-- | Read a value by key.
getReadOpts :: MonadIO m => DB -> ReadOpts -> ByteString -> m (Maybe ByteString)
getReadOpts DB {..} ropts key = liftIO $
    withForeignPtr rocksDB $ \db_ptr ->
    withReadOpts ropts $ \opts_ptr ->
    BU.unsafeUseAsCStringLen key $ \(key_ptr, klen) ->
    alloca                       $ \vlen_ptr -> do
        val_ptr <- throwIfErr "get" $
            c_rocksdb_get db_ptr opts_ptr key_ptr (intToCSize klen) vlen_ptr
        vlen <- peek vlen_ptr
        if val_ptr == nullPtr
            then return Nothing
            else do
                res' <- Just <$> BS.packCStringLen (val_ptr, cSizeToInt vlen)
                freeCString val_ptr
                return res'

-- | Delete a key/value pair.
delete :: MonadIO m => DB -> ByteString -> m ()
delete DB {..} key = liftIO $
    withForeignPtr rocksDB $ \db_ptr ->
    BU.unsafeUseAsCStringLen key $ \(key_ptr, klen) ->
        throwIfErr "delete"
            $ c_rocksdb_delete db_ptr writeOpts key_ptr (intToCSize klen)

-- | Perform a batch mutation.
write :: MonadIO m => DB -> [BatchOp] -> m ()
write DB {..} batch = liftIO $
    withForeignPtr rocksDB $ \db_ptr ->
    bracket c_rocksdb_writebatch_create c_rocksdb_writebatch_destroy $ \batch_ptr -> do

    mapM_ (batchAdd batch_ptr) batch

    throwIfErr "write" $ c_rocksdb_write db_ptr writeOpts batch_ptr

    -- ensure @ByteString@s (and respective shared @CStringLen@s) aren't GC'ed
    -- until here
    mapM_ (liftIO . touch) batch

    where
        batchAdd batch_ptr (Put key val) =
            BU.unsafeUseAsCStringLen key $ \(key_ptr, klen) ->
            BU.unsafeUseAsCStringLen val $ \(val_ptr, vlen) ->
                c_rocksdb_writebatch_put
                batch_ptr
                key_ptr (intToCSize klen)
                val_ptr (intToCSize vlen)

        batchAdd batch_ptr (PutCF cf key val) =
            withForeignPtr cf $ \cf_ptr ->
            BU.unsafeUseAsCStringLen key $ \(key_ptr, klen) ->
            BU.unsafeUseAsCStringLen val $ \(val_ptr, vlen) ->
                c_rocksdb_writebatch_put_cf
                batch_ptr
                cf_ptr
                key_ptr (intToCSize klen)
                val_ptr (intToCSize vlen)

        batchAdd batch_ptr (Del key) =
            BU.unsafeUseAsCStringLen key $ \(key_ptr, klen) ->
                c_rocksdb_writebatch_delete
                batch_ptr
                key_ptr (intToCSize klen)

        batchAdd batch_ptr (DelCF cf key) =
            withForeignPtr cf $ \cf_ptr ->
            BU.unsafeUseAsCStringLen key $ \(key_ptr, klen) ->
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

createBloomFilter :: MonadIO m => Int -> m BloomFilter
createBloomFilter i = liftIO $ do
    fp_ptr <- c_rocksdb_filterpolicy_create_bloom (fromIntegral i)
    newForeignPtr c_rocksdb_filterpolicy_destroy fp_ptr

-- | Marshal a 'FilePath' (Haskell string) into a `NUL` terminated C string using
-- temporary storage.
withFilePath :: FilePath -> (CString -> IO a) -> IO a
withFilePath = GHC.withCString GHC.utf8
