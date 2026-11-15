{-# LANGUAGE LambdaCase #-}

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
-- The API closely follows the C-API of RocksDB. All resources (databases,
-- snapshots, iterators) are managed with bracket-style functions such as
-- 'withDB', 'withSnapshot' and 'withIter', which release them when the
-- supplied action returns or throws. Manual variants such as 'createSnapshot'
-- and 'createIterator' exist for cases where the lifetime of a resource cannot
-- be expressed as a scope.
--
-- A t'DB' handle is safe to use from many threads at once.
module Database.RocksDB.Base
  ( -- * Exported Types
    DB (..),
    BatchOp (..),
    Range,
    ColumnFamily,
    Snapshot,
    ReadOpts,

    -- * Options
    Config (..),
    MergeOperator (..),
    FullMerge,
    PartialMerge,

    -- * Basic Database Manipulations
    withDB,
    withDBCF,
    put,
    putCF,
    delete,
    deleteCF,
    merge,
    mergeCF,
    write,
    get,
    getCF,
    multiGet,
    multiGetCF,
    withSnapshot,
    createSnapshot,
    releaseSnapshot,

    -- * Packed batches
    -- $packed
    writePacked,
    packOps,
    PackedOp (..),
    packedHeaderSize,
    pokePackedHeader,

    -- * Administrative Functions
    Property (..),
    getProperty,
    getPropertyCF,
    destroy,
    repair,
    approximateSize,
    compactRange,
    flush,

    -- * Iteration
    module Database.RocksDB.Iterator,
  )
where

import Control.Exception
import Control.Monad (foldM_, forM, forM_, when)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Internal (ByteString (..))
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Unsafe as BU
import Data.Int
import Data.Maybe (fromMaybe)
import Data.Word (Word32, Word8)
import Database.RocksDB.C
import Database.RocksDB.Internal
import Database.RocksDB.Iterator
import Foreign.C.String
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Utils (copyBytes, withMany)
import Foreign.Ptr
import Foreign.Storable
import System.Directory

-- | Properties exposed by RocksDB. Read them with 'getProperty' or
-- 'getPropertyCF'. Every value is returned by RocksDB as text.
data Property
  = -- | @rocksdb.num-files-at-level\<N\>@: number of SST files at level @N@
    NumFilesAtLevel Int
  | -- | @rocksdb.stats@: multi-line human-readable statistics
    Stats
  | -- | @rocksdb.sstables@: description of the SST files that make up the
    -- database
    SSTables
  | -- | @rocksdb.estimate-table-readers-mem@: memory for index and filter
    -- blocks of open table files, outside the block cache
    EstimateTableReadersMem
  | -- | @rocksdb.cur-size-all-mem-tables@: memory of all memtables
    CurSizeAllMemTables
  | -- | @rocksdb.block-cache-usage@: memory used by the block cache
    BlockCacheUsage
  | -- | any other property, by its full name
    PropertyName String
  deriving (Eq, Show)

-- | A single operation in an atomic batch written with 'write'.
--
-- Batches are how a program keeps several keys consistent with each other: an
-- index and the data it points to, for example, must change together or not at
-- all. RocksDB applies a batch atomically and in one write to its log, so a batch
-- is also much faster than the same writes made one by one.
data BatchOp
  = -- | set a key to a value in the default column family
    Put !ByteString !ByteString
  | -- | delete a key from the default column family
    Del !ByteString
  | -- | set a key to a value in a column family
    PutCF !ColumnFamily !ByteString !ByteString
  | -- | delete a key from a column family
    DelCF !ColumnFamily !ByteString
  | -- | add a merge record to a key of the default column family: the
    -- second byte string is for the t'MergeOperator' of the column family
    -- to combine with the value that the key has. Nothing is read when
    -- the batch is written. RocksDB refuses the batch if the column family
    -- has no merge operator, and 'write' throws. What is left of such a
    -- batch depends on the version: RocksDB 11 keeps none of it, RocksDB
    -- 8.9 keeps the operations that came before the merge. Give every
    -- column family that gets merge records its operator.
    Merge !ByteString !ByteString
  | -- | add a merge record to a key of a column family
    MergeCF !ColumnFamily !ByteString !ByteString
  deriving (Eq, Show)

-- | Open a database at the given path and pass its handle to a function.
--
-- The handle is closed automatically when the function returns or throws, and
-- must not be used afterwards. If 'createIfMissing' is set, the directory is
-- created first if needed. Throws an 'IOError' if the database cannot be
-- opened, for example if it does not exist and 'createIfMissing' is not set,
-- or if it is locked by another process.
--
-- Only the default column family is opened. A database that has other column
-- families must be opened with 'withDBCF'.
--
-- Opening is expensive and a database can only be opened by one process at a
-- time, so open it once at program start and share the t'DB' between threads for
-- the life of the program.
withDB :: FilePath -> Config -> (DB -> IO a) -> IO a
withDB path config f =
  withBlockCache config $ \cache ->
    withOptionsCache cache config $ \opts_ptr ->
      withReadOpts Nothing $ \read_opts ->
        withWriteOpts $ \write_opts ->
          bracket (create_db opts_ptr read_opts write_opts) destroy_db f
  where
    destroy_db db = c_rocksdb_close $ rocksDB db
    create_db opts_ptr read_opts write_opts = do
      when (createIfMissing config) $
        createDirectoryIfMissing True path
      withCString path $ \path_ptr -> do
        db_ptr <-
          throwIfErr "open" $
            c_rocksdb_open opts_ptr path_ptr
        return
          DB
            { rocksDB = db_ptr,
              columnFamilies = [],
              readOpts = read_opts,
              snapshot = Nothing,
              writeOpts = write_opts
            }

-- | Open a database with column families and pass its handle to a function.
--
-- The second argument configures the database and its default column family.
-- The third argument lists the other column families by name, each with its own
-- t'Config'. Their handles are stored in 'columnFamilies', in the same order.
--
-- If the database directory is empty, a new database is created along with all
-- the listed column families. Otherwise the existing database is opened with
-- those column families; if some of them do not exist yet, the open fails unless
-- 'createMissingColumnFamilies' is set in the database t'Config'. RocksDB also
-- requires every column family that exists in the database to be listed.
--
-- The handle and all column family handles are released when the function
-- returns or throws.
--
-- Column families are separate key spaces in one database. They share the
-- write-ahead log, so a batch can change several of them atomically, but each
-- has its own files, memtables and tuning. Use them to keep data of different
-- shapes apart, such as an index with short keys and a table of large values.
withDBCF ::
  FilePath ->
  Config ->
  [(String, Config)] ->
  (DB -> IO a) ->
  IO a
withDBCF path config cf_cfgs f =
  withBlockCache config $ \cache ->
    withOptionsCache cache config $ \opts_ptr ->
      withOptionsCFCache cache (map snd cf_cfgs) $ \cf_opts ->
        withReadOpts Nothing $ \read_opts ->
          withWriteOpts $ \write_opts ->
            withStrings (map fst cf_cfgs) $ \cf_names ->
              allocaArray (length cf_cfgs + 1) $ \cf_names_array ->
                allocaArray (length cf_cfgs + 1) $ \cf_opts_array ->
                  allocaArray (length cf_cfgs + 1) $ \cf_ptrs_array ->
                    bracket
                      ( create_db
                          opts_ptr
                          cf_names
                          cf_names_array
                          cf_opts
                          cf_opts_array
                          read_opts
                          cf_ptrs_array
                          write_opts
                      )
                      destroy_db
                      f
  where
    create_new cf_names cf_opts opts_ptr read_opts write_opts = do
      createDirectoryIfMissing True path
      withCString path $ \path_ptr -> do
        db_ptr <-
          throwIfErr "open" $
            c_rocksdb_open opts_ptr path_ptr
        cfs <- forM (zip cf_names cf_opts) $ \(n, o) ->
          throwIfErr "create_column_family" $
            c_rocksdb_create_column_family
              db_ptr
              o
              n
        return
          DB
            { rocksDB = db_ptr,
              columnFamilies = cfs,
              readOpts = read_opts,
              snapshot = Nothing,
              writeOpts = write_opts
            }
    destroy_db db = do
      mapM_ c_rocksdb_column_family_handle_destroy (columnFamilies db)
      c_rocksdb_close $ rocksDB db
    create_db
      opts_ptr
      cf_names
      cf_names_array
      cf_opts
      cf_opts_array
      read_opts
      cf_ptrs_array
      write_opts = do
        when (createIfMissing config) $
          createDirectoryIfMissing True path
        listDirectory path >>= \case
          [] -> create_new cf_names cf_opts opts_ptr read_opts write_opts
          _ -> withCString path $ \path_ptr ->
            withCString "default" $ \cf_deflt_name -> do
              pokeArray cf_names_array (cf_deflt_name : cf_names)
              pokeArray cf_opts_array (opts_ptr : cf_opts)
              db_ptr <-
                throwIfErr "open" $
                  c_rocksdb_open_column_families
                    opts_ptr
                    path_ptr
                    (intToCInt (length cf_cfgs + 1))
                    cf_names_array
                    cf_opts_array
                    cf_ptrs_array
              (deflt, cfs) <-
                peekArray (length cf_cfgs + 1) cf_ptrs_array >>= \case
                  d : cs -> return (d, cs)
                  [] -> error "rocksdb_open_column_families returned no handles"
              -- Every handle must be destroyed before the database is
              -- closed. The default column family is not kept, so
              -- release its handle now; the database keeps its own.
              c_rocksdb_column_family_handle_destroy deflt
              return
                DB
                  { rocksDB = db_ptr,
                    columnFamilies = cfs,
                    readOpts = read_opts,
                    snapshot = Nothing,
                    writeOpts = write_opts
                  }

-- | Run an action with a snapshot of the database.
-- The t'DB' object is not valid after the action ends.
--
-- The t'DB' passed to the action reads from a consistent point-in-time view of
-- the database: 'get', 'multiGet' and iterators created from it do not see
-- writes made after the snapshot was taken. Writes through it still go to the
-- live database.
--
-- Without a snapshot, two reads made one after the other can see different
-- states of the database if another thread writes in between. Anything that
-- reads several related keys and expects them to agree, such as an index and the
-- records it points to, should read through a snapshot.
withSnapshot :: DB -> (DB -> IO a) -> IO a
withSnapshot db f =
  bracket (createSnapshot db) releaseSnapshot (f . fst)

-- | Manually create an unmanaged snapshot.
-- The returned t'DB' has 'readOpts' configured for the snapshot.
-- Use 'releaseSnapshot' to release both the snapshot and its read options.
createSnapshot :: DB -> IO (DB, Snapshot)
createSnapshot db@DB {rocksDB = db_ptr} = do
  snap_ptr <- c_rocksdb_create_snapshot db_ptr
  read_opts <- createReadOpts (Just snap_ptr)
  return (db {readOpts = read_opts, snapshot = Just snap_ptr}, snap_ptr)

-- | Function to release an unmanaged snapshot.
-- Also releases the read options associated with the snapshot DB.
releaseSnapshot :: (DB, Snapshot) -> IO ()
releaseSnapshot (DB {rocksDB = db_ptr, readOpts = read_opts}, snap_ptr) = do
  destroyReadOpts read_opts
  c_rocksdb_release_snapshot db_ptr snap_ptr

-- | Get a property of the default column family, or of the database as a
-- whole. Returns 'Nothing' if RocksDB does not know the property.
getProperty :: DB -> Property -> IO (Maybe ByteString)
getProperty DB {rocksDB = db_ptr} p =
  withCString (propertyName p) $ \name ->
    c_rocksdb_property_value db_ptr name >>= propertyResult

-- | Get a property of a column family. Many properties, such as the memory
-- of memtables and table readers, are kept per column family.
getPropertyCF :: DB -> ColumnFamily -> Property -> IO (Maybe ByteString)
getPropertyCF DB {rocksDB = db_ptr} cf p =
  withCString (propertyName p) $ \name ->
    c_rocksdb_property_value_cf db_ptr cf name >>= propertyResult

-- | Turn a property value returned by RocksDB into a 'ByteString', freeing the
-- C string.
propertyResult :: CString -> IO (Maybe ByteString)
propertyResult val_ptr
  | val_ptr == nullPtr = return Nothing
  | otherwise = do
      res <- BS.packCString val_ptr
      freeCString val_ptr
      return (Just res)

-- | The name RocksDB uses for a property.
propertyName :: Property -> String
propertyName = \case
  NumFilesAtLevel i -> "rocksdb.num-files-at-level" ++ show i
  Stats -> "rocksdb.stats"
  SSTables -> "rocksdb.sstables"
  EstimateTableReadersMem -> "rocksdb.estimate-table-readers-mem"
  CurSizeAllMemTables -> "rocksdb.cur-size-all-mem-tables"
  BlockCacheUsage -> "rocksdb.block-cache-usage"
  PropertyName n -> n

-- | Destroy the given RocksDB database, deleting all of its files. The
-- database must not be open. Throws an 'IOError' on failure.
destroy :: FilePath -> Options -> IO ()
destroy path opts_ptr =
  withCString path $ \path_ptr ->
    throwIfErr "destroy" $ c_rocksdb_destroy_db opts_ptr path_ptr

-- | Try to recover as much data as possible from a corrupted database. Some
-- data may be lost. The database must not be open. Throws an 'IOError' on
-- failure.
repair :: FilePath -> Options -> IO ()
repair path opts_ptr =
  withCString path $ \path_ptr ->
    throwIfErr "repair" $ c_rocksdb_repair_db opts_ptr path_ptr

-- | A key range given as @(start, limit)@. The start key is included and the
-- limit key is excluded.
--
-- TODO: support [Range], like C API does
type Range = (ByteString, ByteString)

-- | Approximate number of bytes of file system space used by the keys in a
-- 'Range' of the default column family. Data still in memtables may not be
-- counted. Throws an 'IOError' on failure.
approximateSize :: DB -> Range -> IO Int64
approximateSize DB {rocksDB = db_ptr} (from, to) =
  BU.unsafeUseAsCStringLen from $ \(from_ptr, flen) ->
    BU.unsafeUseAsCStringLen to $ \(to_ptr, tlen) ->
      withArray [from_ptr] $ \from_ptrs ->
        withArray [intToCSize flen] $ \flen_ptrs ->
          withArray [to_ptr] $ \to_ptrs ->
            withArray [intToCSize tlen] $ \tlen_ptrs ->
              alloca $ \size_ptr -> do
                throwIfErr "approximate_sizes" $
                  c_rocksdb_approximate_sizes
                    db_ptr
                    1
                    from_ptrs
                    flen_ptrs
                    to_ptrs
                    tlen_ptrs
                    size_ptr
                fromIntegral <$> peek size_ptr

-- | Write again every table file that has keys from the first to the second
-- of the two given, in the default column family or in the one given, and
-- return when it is done. 'Nothing' leaves that end of the range open, so
-- two of them are the whole column family.
--
-- RocksDB compacts by itself, and a program does not need this to keep a
-- database in order. It is for the one thing that only a rewrite does:
-- giving the files that a database already has the layout of its present
-- t'Config', such as a Bloom filter or 'partitionIndexAndFilters', which
-- otherwise reach a file only when RocksDB happens to compact it, and the
-- files of the last level, which hold most of the data, rarely are. Every
-- file in the range is written, those of the last level included.
--
-- It reads and writes all the data in the range, with the background
-- threads of the database ('maxBackgroundJobs'): hours on a large one, and
-- room on the disk for the files being written until the old ones are
-- deleted. The database can be read and written meanwhile. RocksDB reports
-- no error from it; what it could not do stays as it was.
compactRange :: DB -> Maybe ColumnFamily -> Maybe ByteString -> Maybe ByteString -> IO ()
compactRange DB {rocksDB = db_ptr} mcf from to =
  bracket c_rocksdb_compactoptions_create c_rocksdb_compactoptions_destroy $ \opts -> do
    c_rocksdb_compactoptions_set_bottommost_level_compaction opts forceOptimized
    bound from $ \(from_ptr, flen) ->
      bound to $ \(to_ptr, tlen) ->
        case mcf of
          Just cf -> c_rocksdb_compact_range_cf_opt db_ptr cf opts from_ptr flen to_ptr tlen
          Nothing -> c_rocksdb_compact_range_opt db_ptr opts from_ptr flen to_ptr tlen
  where
    -- @BottommostLevelCompaction::kForceOptimized@: the files of the last
    -- level too, but not those that this compaction has just written
    forceOptimized = 3
    bound Nothing f = f (nullPtr, 0)
    bound (Just k) f = BU.unsafeUseAsCStringLen k $ \(p, n) -> f (p, intToCSize n)

-- | Write a key/value pair to the default column family, replacing any
-- previous value. Throws an 'IOError' on failure.
put :: DB -> ByteString -> ByteString -> IO ()
put db = putCommon db Nothing

-- | Like 'put', but write to the given column family.
putCF :: DB -> ColumnFamily -> ByteString -> ByteString -> IO ()
putCF db cf = putCommon db (Just cf)

-- | Write a key/value pair to the default column family, or to a column family
-- if one is given.
putCommon :: DB -> Maybe ColumnFamily -> ByteString -> ByteString -> IO ()
putCommon DB {rocksDB = db_ptr, writeOpts = write_opts} mcf key value =
  BU.unsafeUseAsCStringLen key $ \(key_ptr, klen) ->
    BU.unsafeUseAsCStringLen value $ \(val_ptr, vlen) ->
      throwIfErr "put" $ case mcf of
        Just cf ->
          c_rocksdb_put_cf
            db_ptr
            write_opts
            cf
            key_ptr
            (intToCSize klen)
            val_ptr
            (intToCSize vlen)
        Nothing ->
          c_rocksdb_put
            db_ptr
            write_opts
            key_ptr
            (intToCSize klen)
            val_ptr
            (intToCSize vlen)

-- | Read a value by key from the default column family. Returns 'Nothing' if
-- the key is not found. Throws an 'IOError' on failure.
get :: DB -> ByteString -> IO (Maybe ByteString)
get db = getCommon db Nothing

-- | Like 'get', but read from the given column family.
getCF :: DB -> ColumnFamily -> ByteString -> IO (Maybe ByteString)
getCF db cf = getCommon db (Just cf)

-- | Read a value from the default column family, or from a column family if one
-- is given.
getCommon :: DB -> Maybe ColumnFamily -> ByteString -> IO (Maybe ByteString)
getCommon DB {rocksDB = db_ptr, readOpts = read_opts} mcf key =
  BU.unsafeUseAsCStringLen key $ \(key_ptr, klen) ->
    alloca $ \vlen_ptr -> do
      val_ptr <- throwIfErr "get" $
        case mcf of
          Just cf ->
            c_rocksdb_get_cf
              db_ptr
              read_opts
              cf
              key_ptr
              (intToCSize klen)
              vlen_ptr
          Nothing ->
            c_rocksdb_get
              db_ptr
              read_opts
              key_ptr
              (intToCSize klen)
              vlen_ptr
      if val_ptr == nullPtr
        then return Nothing
        else do
          vlen <- peek vlen_ptr
          res <- BU.unsafePackMallocCStringLen (val_ptr, cSizeToInt vlen)
          return $ Just res

-- | Read the values of many keys in one call. RocksDB looks the keys up
-- together, sharing the work of finding the files and blocks that hold
-- them, which is faster than reading them one by one. The values are in the
-- order of the keys, with 'Nothing' for keys that are not found. Duplicate keys
-- are allowed. Throws an 'IOError' if any of the lookups fails.
--
-- For a query that needs hundreds of keys at once, such as the balances of many
-- addresses, this cuts the time spent finding blocks and crossing into C to a
-- fraction of separate 'get' calls.
multiGet :: DB -> [ByteString] -> IO [Maybe ByteString]
multiGet db = multiGetCommon db Nothing

-- | 'multiGet' on a column family.
multiGetCF :: DB -> ColumnFamily -> [ByteString] -> IO [Maybe ByteString]
multiGetCF db cf = multiGetCommon db (Just cf)

-- | Read many values from the default column family, or from a column family if
-- one is given.
multiGetCommon :: DB -> Maybe ColumnFamily -> [ByteString] -> IO [Maybe ByteString]
multiGetCommon _ _ [] = return []
multiGetCommon DB {rocksDB = db_ptr, readOpts = read_opts} mcf keys =
  withMany BU.unsafeUseAsCStringLen keys $ \kls ->
    withArrayLen (map fst kls) $ \n key_ptrs ->
      withArray (map (intToCSize . snd) kls) $ \key_lens ->
        allocaArray n $ \val_ptrs ->
          allocaArray n $ \val_lens ->
            allocaArray n $ \err_ptrs -> do
              case mcf of
                Just cf ->
                  withArray (replicate n cf) $ \cfs ->
                    c_rocksdb_multi_get_cf
                      db_ptr
                      read_opts
                      cfs
                      (intToCSize n)
                      key_ptrs
                      key_lens
                      val_ptrs
                      val_lens
                      err_ptrs
                Nothing ->
                  c_rocksdb_multi_get
                    db_ptr
                    read_opts
                    (intToCSize n)
                    key_ptrs
                    key_lens
                    val_ptrs
                    val_lens
                    err_ptrs
              vals <- peekArray n val_ptrs
              lens <- peekArray n val_lens
              errs <- peekArray n err_ptrs
              -- Own every value and free every error before throwing, so that
              -- none of them leaks.
              res <- forM (zip vals lens) $ \(v, l) ->
                if v == nullPtr
                  then return Nothing
                  else Just <$> BU.unsafePackMallocCStringLen (v, cSizeToInt l)
              msgs <- forM errs $ \e ->
                if e == nullPtr
                  then return Nothing
                  else do
                    m <- peekCString e
                    free e
                    return (Just m)
              case [m | Just m <- msgs] of
                m : _ -> throwIO $ userError $ "multi_get: " ++ m
                [] -> return res

-- | Delete a key from the default column family. Deleting a key that does not
-- exist is not an error. Throws an 'IOError' on failure.
delete :: DB -> ByteString -> IO ()
delete db = deleteCommon db Nothing

-- | Like 'delete', but delete from the given column family.
deleteCF :: DB -> ColumnFamily -> ByteString -> IO ()
deleteCF db cf = deleteCommon db (Just cf)

-- | Delete a key/value pair from the default column family, or from a column
-- family if one is given.
deleteCommon :: DB -> Maybe ColumnFamily -> ByteString -> IO ()
deleteCommon DB {rocksDB = db_ptr, writeOpts = write_opts} mcf key =
  BU.unsafeUseAsCStringLen key $ \(key_ptr, klen) ->
    throwIfErr "delete" $ case mcf of
      Just cf -> c_rocksdb_delete_cf db_ptr write_opts cf key_ptr (intToCSize klen)
      Nothing -> c_rocksdb_delete db_ptr write_opts key_ptr (intToCSize klen)

-- | Add a merge record to a key of the default column family, for the
-- t'MergeOperator' of the column family to combine with the value that the
-- key has. Nothing is read: the merge is done when the key is read, and
-- when RocksDB compacts the files that hold the record and the value.
--
-- Throws an 'IOError' if the column family has no merge operator, and on
-- any other failure. For many keys at once use a 'Merge' in a batch.
merge :: DB -> ByteString -> ByteString -> IO ()
merge db = mergeCommon db Nothing

-- | Like 'merge', but for a key of the given column family.
mergeCF :: DB -> ColumnFamily -> ByteString -> ByteString -> IO ()
mergeCF db cf = mergeCommon db (Just cf)

-- | Add a merge record to a key of the default column family, or of a
-- column family if one is given.
mergeCommon :: DB -> Maybe ColumnFamily -> ByteString -> ByteString -> IO ()
mergeCommon DB {rocksDB = db_ptr, writeOpts = write_opts} mcf key value =
  BU.unsafeUseAsCStringLen key $ \(key_ptr, klen) ->
    BU.unsafeUseAsCStringLen value $ \(val_ptr, vlen) ->
      throwIfErr "merge" $ case mcf of
        Just cf ->
          c_rocksdb_merge_cf
            db_ptr
            write_opts
            cf
            key_ptr
            (intToCSize klen)
            val_ptr
            (intToCSize vlen)
        Nothing ->
          c_rocksdb_merge
            db_ptr
            write_opts
            key_ptr
            (intToCSize klen)
            val_ptr
            (intToCSize vlen)

-- | Write the memtable of the default column family, or of the one given,
-- to a table file, and return when it is done.
--
-- RocksDB flushes by itself when a memtable is full, and a program does
-- not need this to keep its data: what is in a memtable is in the
-- write-ahead log too. It is for tests and benchmarks that want data in
-- table files, where the data of a database in production is, and with
-- 'disableAutoCompactions' in a file of its own for each flush. Throws an
-- 'IOError' on failure.
flush :: DB -> Maybe ColumnFamily -> IO ()
flush DB {rocksDB = db_ptr} mcf =
  bracket c_rocksdb_flushoptions_create c_rocksdb_flushoptions_destroy $ \opts -> do
    c_rocksdb_flushoptions_set_wait opts 1
    throwIfErr "flush" $ case mcf of
      Just cf -> c_rocksdb_flush_cf db_ptr opts cf
      Nothing -> c_rocksdb_flush db_ptr opts

-- | Perform a batch mutation. All operations in the list are applied atomically:
-- either all of them are written or none are. They are applied in order, so a
-- later operation on a key overrides an earlier one. Throws an 'IOError' on
-- failure.
--
-- The keys and values are kept alive until RocksDB has copied them, because
-- they are handed to C as pointers into Haskell memory.
--
-- Each operation is handed to RocksDB with a foreign call of its own. For a
-- batch of tens of thousands of operations, 'writePacked' hands them over a
-- few thousand at a call.
write :: DB -> [BatchOp] -> IO ()
write DB {rocksDB = db_ptr, writeOpts = write_opts} batch =
  bracket
    c_rocksdb_writebatch_create
    c_rocksdb_writebatch_destroy
    $ \batch_ptr -> do
      mapM_ (batchAdd batch_ptr) batch
      throwIfErr "write" $ c_rocksdb_write db_ptr write_opts batch_ptr
      -- ensure @ByteString@s (and respective shared @CStringLen@s) aren't
      -- GC'ed until here
      mapM_ touch batch
  where
    batchAdd batch_ptr (Put key val) =
      BU.unsafeUseAsCStringLen key $ \(key_ptr, klen) ->
        BU.unsafeUseAsCStringLen val $ \(val_ptr, vlen) ->
          c_rocksdb_writebatch_put
            batch_ptr
            key_ptr
            (intToCSize klen)
            val_ptr
            (intToCSize vlen)
    batchAdd batch_ptr (PutCF cf_ptr key val) =
      BU.unsafeUseAsCStringLen key $ \(key_ptr, klen) ->
        BU.unsafeUseAsCStringLen val $ \(val_ptr, vlen) ->
          c_rocksdb_writebatch_put_cf
            batch_ptr
            cf_ptr
            key_ptr
            (intToCSize klen)
            val_ptr
            (intToCSize vlen)
    batchAdd batch_ptr (Del key) =
      BU.unsafeUseAsCStringLen key $ \(key_ptr, klen) ->
        c_rocksdb_writebatch_delete
          batch_ptr
          key_ptr
          (intToCSize klen)
    batchAdd batch_ptr (DelCF cf_ptr key) =
      BU.unsafeUseAsCStringLen key $ \(key_ptr, klen) ->
        c_rocksdb_writebatch_delete_cf
          batch_ptr
          cf_ptr
          key_ptr
          (intToCSize klen)
    batchAdd batch_ptr (Merge key val) =
      BU.unsafeUseAsCStringLen key $ \(key_ptr, klen) ->
        BU.unsafeUseAsCStringLen val $ \(val_ptr, vlen) ->
          c_rocksdb_writebatch_merge
            batch_ptr
            key_ptr
            (intToCSize klen)
            val_ptr
            (intToCSize vlen)
    batchAdd batch_ptr (MergeCF cf_ptr key val) =
      BU.unsafeUseAsCStringLen key $ \(key_ptr, klen) ->
        BU.unsafeUseAsCStringLen val $ \(val_ptr, vlen) ->
          c_rocksdb_writebatch_merge_cf
            batch_ptr
            cf_ptr
            key_ptr
            (intToCSize klen)
            val_ptr
            (intToCSize vlen)
    touch (Put (PS p _ _) (PS p' _ _)) = do
      touchForeignPtr p
      touchForeignPtr p'
    touch (PutCF _ (PS p _ _) (PS p' _ _)) = do
      touchForeignPtr p
      touchForeignPtr p'
    touch (Del (PS p _ _)) =
      touchForeignPtr p
    touch (DelCF _ (PS p _ _)) =
      touchForeignPtr p
    touch (Merge (PS p _ _) (PS p' _ _)) = do
      touchForeignPtr p
      touchForeignPtr p'
    touch (MergeCF _ (PS p _ _) (PS p' _ _)) = do
      touchForeignPtr p
      touchForeignPtr p'

-- $packed
--
-- 'write' hands a batch to RocksDB one operation at a time, and each is a
-- foreign call of the kind that lets other Haskell threads run meanwhile:
-- the thread gives up its capability for it, and has to get one back. For
-- a batch of a few operations that is nothing. For the 65,000 keys of a
-- Bitcoin block it was more than half of the time of the batch in the
-- benchmark of this package, 26 ms of 48, and 400 ms or more when eight
-- other threads wanted the four capabilities. It comes on top of a
-- 'ByteString' for each key and each value, which is memory that the
-- collector cannot move.
--
-- A packed batch is the same operations written one after another into a
-- few large buffers: /chunks/. 'writePacked' gives each chunk to a small C
-- function of this package, which adds its operations to the batch, and
-- then writes the batch as 'write' does: atomically, with one write.
--
-- There are two ways to make chunks:
--
-- * 'packOps' packs a list of t'BatchOp' into one chunk. It is the simple
--   way, for small batches and for tests.
--
-- * A program that serialises its keys and values itself can do so
--   straight into a buffer of its own, with no 'ByteString' for each: leave
--   'packedHeaderSize' bytes, serialise the key and then the value after
--   them, and fill in the header with 'pokePackedHeader' once the two
--   lengths are known. The next record starts where the value ended.
--
-- A record is a header and then the bytes of the key and of the value, with
-- no padding. The header is the column family handle (a null pointer for
-- the default one), the length of the key and that of the value as 32-bit
-- numbers, and one byte for the operation, all as the machine holds them
-- in memory and none of them aligned. @cbits\/rocksdb_haskell.h@ has the
-- layout for a program that writes records by other means.
--
-- The format is this package's own, and what a chunk becomes is a series of
-- calls to the C API of RocksDB. The alternative was to write RocksDB's own
-- encoding of a batch in Haskell and hand it over whole
-- (@rocksdb_writebatch_create_from@), which saves the copy in C. That
-- encoding is internal to RocksDB, with column family numbers and record
-- types that a binding has no business knowing, and the copy costs a few
-- milliseconds for a batch that takes hundreds to write.
--
-- A chunk holds column family handles, which are pointers: it means
-- something only in the process that made it, and only while the database
-- that the handles came from is open.

-- | What a record of a packed batch does to its key.
data PackedOp
  = -- | set the key to the value, as 'Put' and 'PutCF' do
    PackedPut
  | -- | delete the key, as 'Del' and 'DelCF' do; the record has no value
    PackedDel
  | -- | add a merge record to the key, as 'Merge' and 'MergeCF' do: the
    -- value of the record is for the t'MergeOperator' of the column family
    -- to combine with the value that the key has.
    --
    -- On a column family without a merge operator, RocksDB refuses the
    -- batch when it is written, and 'writePacked' throws. RocksDB 11 then
    -- keeps none of the batch; RocksDB 8.9 keeps the operations that came
    -- before the merge. Do not count on either: a batch that is not all
    -- or nothing is what a batch is there to prevent.
    PackedMerge
  deriving (Eq, Show, Enum, Bounded)

-- | Bytes of the header that comes before the key in a record of a packed
-- batch: 17 where a pointer has eight.
packedHeaderSize :: Int
packedHeaderSize = sizeOf (nullPtr :: ColumnFamily) + 2 * sizeOf (0 :: Word32) + 1

-- | Write the header of a record of a packed batch at a pointer, which
-- needs no alignment: the column family, or 'Nothing' for the default one,
-- the operation, the length of the key and the length of the value. It
-- writes 'packedHeaderSize' bytes. The bytes of the key come right after
-- them, and those of the value right after the key.
--
-- It is for a program that serialises keys and values into its own buffer:
-- see the top of this section. The lengths are only known once that is
-- done, so the header is written last, into the room left for it.
--
-- Throws an 'IOError', and writes nothing, if a length is negative or does
-- not fit in 32 bits, or if a 'PackedDel' is given a value.
pokePackedHeader :: Ptr a -> Maybe ColumnFamily -> PackedOp -> Int -> Int -> IO ()
pokePackedHeader ptr mcf op klen vlen = do
  when (outside klen || outside vlen) $
    throwIO (userError "pokePackedHeader: a length that does not fit in 32 bits")
  when (op == PackedDel && vlen /= 0) $
    throwIO (userError "pokePackedHeader: a delete with a value")
  -- Unaligned writes, in the byte order of the machine, which is how
  -- the C side reads them back with memcpy.
  poke (castPtr ptr) (fromMaybe nullPtr mcf)
  pokeByteOff ptr word (fromIntegral klen :: Word32)
  pokeByteOff ptr (word + 4) (fromIntegral vlen :: Word32)
  pokeByteOff ptr (word + 8) code
  where
    word = sizeOf (nullPtr :: ColumnFamily)
    -- compared as words, which have 32 bits at least, so that the limit is
    -- not a negative number where an 'Int' has 32
    outside n = n < 0 || (fromIntegral n :: Word) > fromIntegral (maxBound :: Word32)
    -- ROCKSDB_HASKELL_PUT, _DELETE and _MERGE of rocksdb_haskell.h
    code :: Word8
    code = case op of
      PackedPut -> 0
      PackedDel -> 1
      PackedMerge -> 2
{-# INLINE pokePackedHeader #-}

-- | Pack the operations of a batch into one chunk for 'writePacked', in
-- their order. @'writePacked' db ['packOps' ops]@ does what @'write' db ops@
-- does.
--
-- This is the simple way to a packed batch: it copies every key and value
-- once more, into a buffer as large as all of them together. Use it for
-- small batches, for the odd operation that a program does not serialise
-- itself, and for tests. A program with a large batch gains more by
-- serialising straight into its own chunks with 'pokePackedHeader'.
--
-- The chunk holds the column family handles of the operations, so it is
-- only good while their database is open. Evaluating it throws an 'IOError'
-- if a key or a value is of four gigabytes or more.
packOps :: [BatchOp] -> ByteString
packOps ops = BI.unsafeCreate (sum (map size ops)) $ \start -> foldM_ pack start ops
  where
    parts (Put k v) = (Nothing, PackedPut, k, v)
    parts (Del k) = (Nothing, PackedDel, k, BS.empty)
    parts (PutCF cf k v) = (Just cf, PackedPut, k, v)
    parts (DelCF cf k) = (Just cf, PackedDel, k, BS.empty)
    parts (Merge k v) = (Nothing, PackedMerge, k, v)
    parts (MergeCF cf k v) = (Just cf, PackedMerge, k, v)
    size op = case parts op of
      (_, _, k, v) -> packedHeaderSize + BS.length k + BS.length v
    -- write the record of an operation, and give where the next one goes
    pack :: Ptr Word8 -> BatchOp -> IO (Ptr Word8)
    pack ptr op = do
      let (mcf, kind, k, v) = parts op
          key_ptr = ptr `plusPtr` packedHeaderSize
          val_ptr = key_ptr `plusPtr` BS.length k
      pokePackedHeader ptr mcf kind (BS.length k) (BS.length v)
      copy k key_ptr
      copy v val_ptr
      return (val_ptr `plusPtr` BS.length v)
    -- memcpy must not be given the pointer of an empty string
    copy bytes to =
      BU.unsafeUseAsCStringLen bytes $ \(from, len) ->
        when (len > 0) $ copyBytes to (castPtr from) len

-- | Write a batch that is given as packed chunks: see the top of this
-- section for what they are and how to make them. All the operations of all
-- the chunks are applied atomically, as one batch and with one write:
-- either all of them are written or none are. They are applied in the order
-- of the chunks, and within a chunk in the order of its records, so a later
-- operation on a key overrides an earlier one.
--
-- Every chunk must hold whole records: a record cannot start in one chunk
-- and end in the next. Where a chunk is cut otherwise does not matter, and
-- an empty chunk or no chunk at all is fine. So a program that packs on
-- several cores gives the chunks of each core, one core after another.
--
-- Use it for batches of thousands of operations, where it saves the
-- foreign call that 'write' makes for each operation, and the 'ByteString'
-- for each key and value if the program packs its own chunks. It makes one
-- foreign call for each chunk, so chunks of a few hundred kilobytes are
-- large enough. For a few operations, 'write' is as fast and simpler.
--
-- Throws an 'IOError', with nothing written, if a chunk is not made of
-- whole records: one that is cut short, an operation that does not exist,
-- a delete with a value. The message names the chunk and the record, both
-- counted from zero. Each chunk is checked in C before any of it is added
-- to the batch, and nothing is read outside it. What cannot be checked is
-- the column family handle of a record: a wrong one crashes the process,
-- as it would in 'PutCF'.
--
-- Also throws an 'IOError' if RocksDB refuses the batch, as 'write' does.
-- That is what happens to a batch with a 'PackedMerge' for a column family
-- that has no 'mergeOperator': RocksDB 11 refuses it when it is written,
-- with nothing written.
writePacked :: DB -> [ByteString] -> IO ()
writePacked DB {rocksDB = db_ptr, writeOpts = write_opts} chunks =
  bracket
    c_rocksdb_writebatch_create
    c_rocksdb_writebatch_destroy
    $ \batch_ptr -> do
      forM_ (zip [0 :: Int ..] chunks) $ \(n, chunk) ->
        -- the chunk cannot move or be freed during the call, and RocksDB
        -- has copied what it needs when the call returns
        BU.unsafeUseAsCStringLen chunk $ \(chunk_ptr, len) ->
          alloca $ \count_ptr -> do
            status <-
              c_rocksdb_haskell_writebatch_add_packed
                batch_ptr
                chunk_ptr
                (intToCSize len)
                count_ptr
            when (status /= 0) $ do
              count <- peek count_ptr
              -- both are counted from zero
              throwIO . userError . unwords $
                [ "writePacked: chunk",
                  show n,
                  "is not made of whole records: record",
                  show (cSizeToInt count),
                  "is malformed or cut short"
                ]
      throwIfErr "write" $ c_rocksdb_write db_ptr write_opts batch_ptr

-- | Marshal a list of strings into C strings that are valid while the function
-- runs.
withStrings :: [String] -> ([CString] -> IO a) -> IO a
withStrings ss f =
  go [] ss
  where
    go acc [] = f (reverse acc)
    go acc (x : xs) = withCString x $ \p -> go (p : acc) xs
