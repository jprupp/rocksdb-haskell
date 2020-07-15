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
    , defaultReadOpts
    , WriteOpts
    , newWriteOpts
    , defaultWriteOpts

    -- * Basic Database Manipulations
    , open
    , withRocksDB
    , close
    , put
    , putBinaryVal
    , putBinary
    , delete
    , write
    , get
    , getBinary
    , getBinaryVal
    , withSnapshot
    , createSnapshot
    , releaseSnapshot

    -- * Filter Policy / Bloom Filter
    , BloomFilter
    , createBloomFilter
    , bloomFilter

    -- * Administrative Functions
    , Property (..), getProperty
    , destroy
    , repair
    , approximateSize

    -- * Utility functions to help perform mass writes
    , binaryToBS
    , bsToBinary

    -- * Iteration
    , module Database.RocksDB.Iterator
    ) where

import           Control.Monad             (when, (>=>))
import           Data.Binary               (Binary)
import qualified Data.Binary               as Binary
import           Data.ByteString           (ByteString)
import qualified Data.ByteString           as BS
import           Data.ByteString.Internal  (ByteString (..))
import qualified Data.ByteString.Lazy      as BSL
import qualified Data.ByteString.Unsafe    as BU
import           Database.RocksDB.C
import           Database.RocksDB.Internal
import           Database.RocksDB.Iterator
import           Foreign
import           Foreign.C.String          (CString, withCString)
import qualified GHC.Foreign               as GHC
import qualified GHC.IO.Encoding           as GHC
import           System.Directory          (createDirectoryIfMissing)
import           UnliftIO

type BloomFilter = ForeignPtr LBloomFilter

-- | Properties exposed by RocksDB
data Property = NumFilesAtLevel Int | Stats | SSTables
    deriving (Eq, Show)

data BatchOp = Put !ByteString !ByteString
             | Del !ByteString
             | PutCF !ColumnFamily !ByteString !ByteString
             | DelCF !ColumnFamily !ByteString
             deriving (Eq, Show)

data DB = DB { rocksDB        :: !RocksDB
             , options        :: !Options
             , columnFamilies :: ![(ColumnFamily, Options)]
             }

-- | Create a 'BloomFilter'
bloomFilter :: MonadIO m => Int -> m BloomFilter
bloomFilter i = liftIO $ do
    bloom_ptr <- c_rocksdb_filterpolicy_create_bloom (intToCInt i)
    newForeignPtr c_rocksdb_filterpolicy_destroy bloom_ptr

withRocksDB :: MonadUnliftIO m => FilePath -> Options -> (DB -> m a) -> m a
withRocksDB path opts = bracket (open path opts) close

-- | Open a database.
--
-- The returned handle should be released with 'close'.
open :: MonadIO m => FilePath -> Options -> m DB
open path opts@Options {..} = liftIO $ do
    when (createIfMissing config) $ createDirectoryIfMissing True path
    withFilePath path $ \path_ptr ->
        withForeignPtr options $ \opts_ptr -> do
            db_ptr <- throwIfErr "open" $ c_rocksdb_open opts_ptr path_ptr
            db_fptr <- newForeignPtr c_rocksdb_close db_ptr
            return $ DB db_fptr opts []

-- | Close a database.
--
-- The handle will be invalid after calling this action and should no
-- longer be used.
close :: MonadIO m => DB -> m ()
close DB {..} = liftIO $ do
    mapM_ (finalizeForeignPtr . fst) columnFamilies
    finalizeForeignPtr rocksDB


-- | Run an action with a 'Snapshot' of the database.
withSnapshot :: MonadUnliftIO m => DB -> (Snapshot -> m a) -> m a
withSnapshot db =
    bracket (createSnapshot db) (releaseSnapshot db)

-- | Create a snapshot of the database.
--
-- The returned 'Snapshot' should be released with 'releaseSnapshot'.
createSnapshot :: MonadIO m => DB -> m Snapshot
createSnapshot DB {..} =
    liftIO $ withForeignPtr rocksDB $ \db_ptr ->
    c_rocksdb_create_snapshot db_ptr

-- | Release a snapshot.
--
-- The handle will be invalid after calling this action and should no
-- longer be used.
releaseSnapshot :: MonadIO m => DB -> Snapshot -> m ()
releaseSnapshot DB {..} snap =
    liftIO $ withForeignPtr rocksDB $ \db_ptr ->
    c_rocksdb_release_snapshot db_ptr snap

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
    withForeignPtr options $ \opts_ptr ->
    throwIfErr "destroy" $ c_rocksdb_destroy_db opts_ptr path_ptr

-- | Repair the given RocksDB database.
repair :: MonadIO m => FilePath -> Options -> m ()
repair path Options {..} = liftIO $
    withFilePath path $ \path_ptr ->
    withForeignPtr options $ \opts_ptr ->
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

putBinaryVal :: (MonadIO m, Binary v) => DB -> WriteOpts -> ByteString -> v -> m ()
putBinaryVal db wopts key val = put db wopts key (binaryToBS val)

putBinary :: (MonadIO m, Binary k, Binary v) => DB -> WriteOpts -> k -> v -> m ()
putBinary db wopts key val = put db wopts (binaryToBS key) (binaryToBS val)

-- | Write a key/value pair.
put :: MonadIO m => DB -> WriteOpts -> ByteString -> ByteString -> m ()
put DB {..} opts key value = liftIO $
    withForeignPtr rocksDB $ \db_ptr ->
    withForeignPtr opts $ \opts_ptr ->
    BU.unsafeUseAsCStringLen key   $ \(key_ptr, klen) ->
    BU.unsafeUseAsCStringLen value $ \(val_ptr, vlen) ->
        throwIfErr "put"
            $ c_rocksdb_put db_ptr opts_ptr
                            key_ptr (intToCSize klen)
                            val_ptr (intToCSize vlen)

getBinaryVal :: (Binary v, MonadIO m) => DB -> ReadOpts -> ByteString -> m (Maybe v)
getBinaryVal db ropts key  = fmap bsToBinary <$> get db ropts key

getBinary :: (MonadIO m, Binary k, Binary v) => DB -> ReadOpts -> k -> m (Maybe v)
getBinary db ropts key = fmap bsToBinary <$> get db ropts (binaryToBS key)

-- | Read a value by key.
get :: MonadIO m => DB -> ReadOpts -> ByteString -> m (Maybe ByteString)
get DB {..} opts key = liftIO $
    withForeignPtr rocksDB $ \db_ptr ->
    withForeignPtr opts $ \opts_ptr ->
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
delete :: MonadIO m => DB -> WriteOpts -> ByteString -> m ()
delete DB {..} opts key = liftIO $
    withForeignPtr rocksDB $ \db_ptr ->
    withForeignPtr opts $ \opts_ptr ->
    BU.unsafeUseAsCStringLen key $ \(key_ptr, klen) ->
        throwIfErr "delete"
            $ c_rocksdb_delete db_ptr opts_ptr key_ptr (intToCSize klen)

-- | Perform a batch mutation.
write :: MonadIO m => DB -> WriteOpts -> [BatchOp] -> m ()
write DB {..} opts batch = liftIO $
    withForeignPtr rocksDB $ \db_ptr ->
    withForeignPtr opts $ \opts_ptr ->
    bracket c_rocksdb_writebatch_create c_rocksdb_writebatch_destroy $ \batch_ptr -> do

    mapM_ (batchAdd batch_ptr) batch

    throwIfErr "write" $ c_rocksdb_write db_ptr opts_ptr batch_ptr

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

binaryToBS :: Binary v => v -> ByteString
binaryToBS x = BSL.toStrict (Binary.encode x)

bsToBinary :: Binary v => ByteString -> v
bsToBinary x = Binary.decode (BSL.fromStrict x)

-- | Marshal a 'FilePath' (Haskell string) into a `NUL` terminated C string using
-- temporary storage.
withFilePath :: FilePath -> (CString -> IO a) -> IO a
withFilePath = GHC.withCString GHC.utf8
