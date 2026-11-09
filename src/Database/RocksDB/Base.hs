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
-- A 'DB' handle is safe to use from many threads at once.

module Database.RocksDB.Base
    ( -- * Exported Types
      DB (..)
    , BatchOp (..)
    , Range
    , ColumnFamily
    , Snapshot
    , ReadOpts

    -- * Options
    , Config (..)

    -- * Basic Database Manipulations
    , withDB
    , withDBCF
    , put
    , putCF
    , delete
    , deleteCF
    , write
    , get
    , getCF
    , multiGet
    , multiGetCF
    , withSnapshot
    , createSnapshot
    , releaseSnapshot

    -- * Administrative Functions
    , Property (..), getProperty, getPropertyCF
    , destroy
    , repair
    , approximateSize

    -- * Iteration
    , module Database.RocksDB.Iterator
    ) where

import           Control.Exception
import           Control.Monad             (forM, when, (>=>))
import           Data.ByteString           (ByteString)
import qualified Data.ByteString           as BS
import           Data.ByteString.Internal  (ByteString (..))
import qualified Data.ByteString.Unsafe    as BU
import           Data.Int
import           Database.RocksDB.C
import           Database.RocksDB.Internal
import           Database.RocksDB.Iterator
import           Foreign.C.String
import           Foreign.ForeignPtr
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Array
import           Foreign.Marshal.Utils     (withMany)
import           Foreign.Ptr
import           Foreign.Storable
import           System.Directory

-- | Properties exposed by RocksDB. Read them with 'getProperty' or
-- 'getPropertyCF'. Every value is returned by RocksDB as text.
data Property
    -- | @rocksdb.num-files-at-level\<N\>@: number of SST files at level @N@
    = NumFilesAtLevel Int
    -- | @rocksdb.stats@: multi-line human-readable statistics
    | Stats
    -- | @rocksdb.sstables@: description of the SST files that make up the
    -- database
    | SSTables
    -- | @rocksdb.estimate-table-readers-mem@: memory for index and filter
    -- blocks of open table files, outside the block cache
    | EstimateTableReadersMem
    -- | @rocksdb.cur-size-all-mem-tables@: memory of all memtables
    | CurSizeAllMemTables
    -- | @rocksdb.block-cache-usage@: memory used by the block cache
    | BlockCacheUsage
    -- | any other property, by its full name
    | PropertyName String
    deriving (Eq, Show)

-- | A single operation in an atomic batch written with 'write'.
--
-- Batches are how a program keeps several keys consistent with each other: an
-- index and the data it points to, for example, must change together or not at
-- all. RocksDB applies a batch atomically and in one write to its log, so a batch
-- is also much faster than the same writes made one by one.
data BatchOp = Put !ByteString !ByteString
               -- ^ set a key to a value in the default column family
             | Del !ByteString
               -- ^ delete a key from the default column family
             | PutCF !ColumnFamily !ByteString !ByteString
               -- ^ set a key to a value in a column family
             | DelCF !ColumnFamily !ByteString
               -- ^ delete a key from a column family
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
-- time, so open it once at program start and share the 'DB' between threads for
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
            db_ptr <- throwIfErr "open" $
                c_rocksdb_open opts_ptr path_ptr
            return DB { rocksDB = db_ptr
                      , columnFamilies = []
                      , readOpts = read_opts
                      , writeOpts = write_opts
                      }

-- | Open a database with column families and pass its handle to a function.
--
-- The second argument configures the database and its default column family.
-- The third argument lists the other column families by name, each with its own
-- 'Config'. Their handles are stored in 'columnFamilies', in the same order.
--
-- If the database directory is empty, a new database is created along with all
-- the listed column families. Otherwise the existing database is opened with
-- those column families; if some of them do not exist yet, the open fails unless
-- 'createMissingColumnFamilies' is set in the database 'Config'. RocksDB also
-- requires every column family that exists in the database to be listed.
--
-- The handle and all column family handles are released when the function
-- returns or throws.
--
-- Column families are separate key spaces in one database. They share the
-- write-ahead log, so a batch can change several of them atomically, but each
-- has its own files, memtables and tuning. Use them to keep data of different
-- shapes apart, such as an index with short keys and a table of large values.
withDBCF :: FilePath
         -> Config
         -> [(String, Config)]
         -> (DB -> IO a)
         -> IO a
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
        bracket ( create_db opts_ptr
                            cf_names
                            cf_names_array
                            cf_opts
                            cf_opts_array
                            read_opts
                            cf_ptrs_array
                            write_opts
                ) destroy_db f
  where
    create_new cf_names cf_opts opts_ptr read_opts write_opts = do
        createDirectoryIfMissing True path
        withCString path $ \path_ptr -> do
            db_ptr <- throwIfErr "open" $
                c_rocksdb_open opts_ptr path_ptr
            cfs <- forM (zip cf_names cf_opts) $ \(n, o) ->
                throwIfErr "create_column_family" $
                c_rocksdb_create_column_family
                db_ptr o n
            return DB { rocksDB = db_ptr
                      , columnFamilies = cfs
                      , readOpts = read_opts
                      , writeOpts = write_opts
                      }
    destroy_db db = do
        mapM_ c_rocksdb_column_family_handle_destroy (columnFamilies db)
        c_rocksdb_close $ rocksDB db
    create_db opts_ptr
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
                    db_ptr <- throwIfErr "open" $
                        c_rocksdb_open_column_families
                        opts_ptr
                        path_ptr
                        (intToCInt (length cf_cfgs + 1))
                        cf_names_array
                        cf_opts_array
                        cf_ptrs_array
                    (deflt, cfs) <- peekArray (length cf_cfgs + 1) cf_ptrs_array >>= \case
                        d : cs -> return (d, cs)
                        [] -> error "rocksdb_open_column_families returned no handles"
                    -- Every handle must be destroyed before the database is
                    -- closed. The default column family is not kept, so
                    -- release its handle now; the database keeps its own.
                    c_rocksdb_column_family_handle_destroy deflt
                    return DB { rocksDB = db_ptr
                              , columnFamilies = cfs
                              , readOpts = read_opts
                              , writeOpts = write_opts
                              }

-- | Run an action with a snapshot of the database.
-- The 'DB' object is not valid after the action ends.
--
-- The 'DB' passed to the action reads from a consistent point-in-time view of
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
-- The returned 'DB' has 'readOpts' configured for the snapshot.
-- Use 'releaseSnapshot' to release both the snapshot and its read options.
createSnapshot :: DB -> IO (DB, Snapshot)
createSnapshot db@DB{rocksDB = db_ptr} = do
    snap_ptr <- c_rocksdb_create_snapshot db_ptr
    read_opts <- createReadOpts (Just snap_ptr)
    return (db{readOpts = read_opts}, snap_ptr)

-- | Function to release an unmanaged snapshot.
-- Also releases the read options associated with the snapshot DB.
releaseSnapshot :: (DB, Snapshot) -> IO ()
releaseSnapshot (DB{rocksDB = db_ptr, readOpts = read_opts}, snap_ptr) = do
  destroyReadOpts read_opts
  c_rocksdb_release_snapshot db_ptr snap_ptr

-- | Get a property of the default column family, or of the database as a
-- whole. Returns 'Nothing' if RocksDB does not know the property.
getProperty :: DB -> Property -> IO (Maybe ByteString)
getProperty DB{rocksDB = db_ptr} p =
    withCString (propertyName p) $ \name ->
    c_rocksdb_property_value db_ptr name >>= propertyResult

-- | Get a property of a column family. Many properties, such as the memory
-- of memtables and table readers, are kept per column family.
getPropertyCF :: DB -> ColumnFamily -> Property -> IO (Maybe ByteString)
getPropertyCF DB{rocksDB = db_ptr} cf p =
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
    NumFilesAtLevel i       -> "rocksdb.num-files-at-level" ++ show i
    Stats                   -> "rocksdb.stats"
    SSTables                -> "rocksdb.sstables"
    EstimateTableReadersMem -> "rocksdb.estimate-table-readers-mem"
    CurSizeAllMemTables     -> "rocksdb.cur-size-all-mem-tables"
    BlockCacheUsage         -> "rocksdb.block-cache-usage"
    PropertyName n          -> n

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
type Range  = (ByteString, ByteString)

-- | Approximate number of bytes of file system space used by the keys in a
-- 'Range' of the default column family. Data still in memtables may not be
-- counted. Throws an 'IOError' on failure.
approximateSize :: DB -> Range -> IO Int64
approximateSize DB{rocksDB = db_ptr} (from, to) =
    BU.unsafeUseAsCStringLen from $ \(from_ptr, flen) ->
    BU.unsafeUseAsCStringLen to   $ \(to_ptr, tlen)   ->
    withArray [from_ptr]          $ \from_ptrs        ->
    withArray [intToCSize flen]   $ \flen_ptrs        ->
    withArray [to_ptr]            $ \to_ptrs          ->
    withArray [intToCSize tlen]   $ \tlen_ptrs        ->
    alloca                        $ \size_ptr         -> do
        throwIfErr "approximate_sizes" $
            c_rocksdb_approximate_sizes db_ptr 1
                                        from_ptrs flen_ptrs
                                        to_ptrs tlen_ptrs
                                        size_ptr
        fromIntegral <$> peek size_ptr

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
putCommon DB{rocksDB = db_ptr, writeOpts = write_opts} mcf key value =
    BU.unsafeUseAsCStringLen key   $ \(key_ptr, klen) ->
    BU.unsafeUseAsCStringLen value $ \(val_ptr, vlen) ->
        throwIfErr "put" $ case mcf of
            Just cf -> c_rocksdb_put_cf
                      db_ptr write_opts cf
                      key_ptr (intToCSize klen)
                      val_ptr (intToCSize vlen)
            Nothing -> c_rocksdb_put
                      db_ptr write_opts
                      key_ptr (intToCSize klen)
                      val_ptr (intToCSize vlen)

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
getCommon DB{rocksDB = db_ptr, readOpts = read_opts} mcf key =
    BU.unsafeUseAsCStringLen key $ \(key_ptr, klen) ->
    alloca                       $ \vlen_ptr -> do
        val_ptr <- throwIfErr "get" $
            case mcf of
                Just cf -> c_rocksdb_get_cf
                           db_ptr read_opts cf
                           key_ptr (intToCSize klen) vlen_ptr
                Nothing -> c_rocksdb_get
                           db_ptr read_opts
                           key_ptr (intToCSize klen) vlen_ptr
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
multiGetCommon DB{rocksDB = db_ptr, readOpts = read_opts} mcf keys =
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
                    db_ptr read_opts cfs (intToCSize n)
                    key_ptrs key_lens val_ptrs val_lens err_ptrs
            Nothing ->
                c_rocksdb_multi_get
                    db_ptr read_opts (intToCSize n)
                    key_ptrs key_lens val_ptrs val_lens err_ptrs
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
deleteCommon DB{rocksDB = db_ptr, writeOpts = write_opts} mcf key =
    BU.unsafeUseAsCStringLen key $ \(key_ptr, klen) ->
    throwIfErr "delete" $ case mcf of
    Just cf -> c_rocksdb_delete_cf db_ptr write_opts cf key_ptr (intToCSize klen)
    Nothing -> c_rocksdb_delete db_ptr write_opts key_ptr (intToCSize klen)

-- | Perform a batch mutation. All operations in the list are applied atomically:
-- either all of them are written or none are. They are applied in order, so a
-- later operation on a key overrides an earlier one. Throws an 'IOError' on
-- failure.
--
-- The keys and values are kept alive until RocksDB has copied them, because
-- they are handed to C as pointers into Haskell memory.
write :: DB -> [BatchOp] -> IO ()
write DB{rocksDB = db_ptr, writeOpts = write_opts} batch =
    bracket
    c_rocksdb_writebatch_create
    c_rocksdb_writebatch_destroy $ \batch_ptr -> do
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
            key_ptr (intToCSize klen)
            val_ptr (intToCSize vlen)
    batchAdd batch_ptr (PutCF cf_ptr key val) =
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
    batchAdd batch_ptr (DelCF cf_ptr key) =
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
    touch (Del (PS p _ _)) =
        touchForeignPtr p
    touch (DelCF _ (PS p _ _)) =
        touchForeignPtr p

-- | Marshal a list of strings into C strings that are valid while the function
-- runs.
withStrings :: [String] -> ([CString] -> IO a) -> IO a
withStrings ss f =
    go [] ss
  where
    go acc []     = f (reverse acc)
    go acc (x:xs) = withCString x $ \p -> go (p:acc) xs
