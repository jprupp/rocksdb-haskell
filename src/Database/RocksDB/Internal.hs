{-# LANGUAGE RecordWildCards #-}
-- |
-- Module      : Database.RocksDB.Internal
-- Copyright   : (c) 2012-2013 The leveldb-haskell Authors
--               (c) 2014-2020 The rocksdb-haskell Authors
-- License     : BSD3
-- Maintainer  : jprupp@protonmail.ch
-- Stability   : experimental
-- Portability : non-portable
--
-- Internal types and helpers shared by the other modules. The 'DB' and 'Config'
-- types are re-exported by "Database.RocksDB"; everything else is an
-- implementation detail.

module Database.RocksDB.Internal
    ( Config (..)
    , DB (..)

    -- * Smart constructors & extractors
    , withOptions
    , withOptionsCache
    , withOptionsCF
    , withOptionsCFCache
    , withBlockCache
    , withReadOpts
    , createReadOpts
    , destroyReadOpts
    , withWriteOpts

    -- * Utilities
    , freeCString
    , throwIfErr
    , cSizeToInt
    , intToCSize
    , intToCInt
    , cIntToInt
    , boolToNum
    ) where

import           Control.Exception
import           Control.Monad
import           Data.Default
import           Data.Maybe        (isJust)
import           Database.RocksDB.C
import           Foreign
import           Foreign.C.String
import           Foreign.C.Types

-- | Handle to an open database. Obtain one with 'Database.RocksDB.withDB' or
-- 'Database.RocksDB.withDBCF'. It is only valid inside the function passed to
-- those, and can be shared between threads.
data DB = DB { rocksDB        :: !RocksDB
               -- ^ pointer to the C database object
             , columnFamilies :: ![ColumnFamily]
               -- ^ handles of the column families opened with
               -- 'Database.RocksDB.withDBCF', in the order they were given;
               -- empty for 'Database.RocksDB.withDB'
             , readOpts       :: !ReadOpts
               -- ^ read options used by reads and iterators; set to read from
               -- a snapshot by 'Database.RocksDB.withSnapshot'
             , writeOpts      :: !WriteOpts
               -- ^ write options used by all writes
             }

-- | Configuration of a database or column family. Use 'def' for the defaults
-- and override the fields you need:
--
-- @
-- def { 'createIfMissing' = True, 'bloomFilter' = True }
-- @
--
-- Some fields only apply to the database as a whole and are ignored in the
-- configuration of a column family. This is noted on each field.
data Config = Config { createIfMissing :: !Bool
                       -- ^ create the database if it does not exist
                     , errorIfExists   :: !Bool
                       -- ^ fail to open if the database already exists
                     , paranoidChecks  :: !Bool
                       -- ^ check data aggressively and stop early on any
                       -- corruption
                     , createMissingColumnFamilies :: !Bool
                       -- ^ create the column families passed to 'withDBCF'
                       -- that an existing database does not have yet
                     , maxFiles        :: !(Maybe Int)
                       -- ^ maximum number of files RocksDB keeps open
                       -- ('Nothing': RocksDB default, unlimited)
                     , prefixLength    :: !(Maybe Int)
                       -- ^ length in bytes of a fixed key prefix, used by
                       -- bloom filters and prefix seeks
                     , bloomFilter     :: !Bool
                       -- ^ use a full bloom filter with 10 bits per key to
                       -- skip files that cannot contain a key
                     , writeBufferSize :: !(Maybe Int)
                       -- ^ bytes per memtable (RocksDB default 64 MiB);
                       -- per column family
                     , dbWriteBufferSize :: !(Maybe Int)
                       -- ^ bytes for all memtables across all column
                       -- families (RocksDB default unlimited); database-wide,
                       -- ignored in column family configs
                     , blockCacheSize :: !(Maybe Int)
                       -- ^ bytes of a least-recently-used block cache shared
                       -- by the database and all its column families
                       -- ('Nothing': RocksDB gives each column family its
                       -- own default cache, of 32 MiB in RocksDB 8);
                       -- database-wide, ignored in column family configs
                     , cacheIndexAndFilterBlocks :: !Bool
                       -- ^ keep index and filter blocks in the block cache,
                       -- at high priority, with those of level 0 pinned,
                       -- instead of in memory outside it for every open
                       -- file; per column family
                     , maxTotalWalSize :: !(Maybe Int)
                       -- ^ bytes of write-ahead log after which the column
                       -- families holding the oldest log are flushed, so it
                       -- can be deleted (RocksDB default: four times the
                       -- memtables); database-wide, ignored in column family
                       -- configs
                     } deriving (Eq, Show)

-- | All flags off and all sizes left to RocksDB defaults.
instance Default Config where
    def = Config { createIfMissing  = False
                 , errorIfExists    = False
                 , paranoidChecks   = False
                 , createMissingColumnFamilies = False
                 , maxFiles         = Nothing
                 , prefixLength     = Nothing
                 , bloomFilter      = False
                 , writeBufferSize  = Nothing
                 , dbWriteBufferSize = Nothing
                 , blockCacheSize   = Nothing
                 , cacheIndexAndFilterBlocks = False
                 , maxTotalWalSize  = Nothing
                 }

-- | Create RocksDB options from a 'Config' and pass them to a function. The
-- options are destroyed when it returns.
withOptions :: Config -> (Options -> IO a) -> IO a
withOptions = withOptionsCache Nothing

-- | Options of a database or column family that use this block cache, if
-- any. The options are destroyed when the function returns.
withOptionsCache :: Maybe Cache -> Config -> (Options -> IO a) -> IO a
withOptionsCache cache Config {..} f = with_opts $ \opts -> do
    when (bloomFilter || isJust cache || cacheIndexAndFilterBlocks) $
        bracket c_rocksdb_block_based_options_create
                c_rocksdb_block_based_options_destroy $ \bo -> do
            when bloomFilter $ do
                fp <- c_rocksdb_filterpolicy_create_bloom_full 10
                c_rocksdb_block_based_options_set_filter_policy bo fp
            forM_ cache $ c_rocksdb_block_based_options_set_block_cache bo
            when cacheIndexAndFilterBlocks $ do
                c_rocksdb_block_based_options_set_cache_index_and_filter_blocks bo 1
                c_rocksdb_block_based_options_set_cache_index_and_filter_blocks_with_high_priority bo 1
                c_rocksdb_block_based_options_set_pin_l0_filter_and_index_blocks_in_cache bo 1
            -- the table factory copies the options, filter policy and cache
            c_rocksdb_options_set_block_based_table_factory opts bo
    forM_ prefixLength $ \l -> do
        t <- c_rocksdb_slicetransform_create_fixed_prefix (intToCSize l)
        c_rocksdb_options_set_prefix_extractor opts t
    forM_ maxFiles $
        c_rocksdb_options_set_max_open_files opts . intToCInt
    forM_ writeBufferSize $
        c_rocksdb_options_set_write_buffer_size opts . intToCSize
    forM_ dbWriteBufferSize $
        c_rocksdb_options_set_db_write_buffer_size opts . intToCSize
    forM_ maxTotalWalSize $
        c_rocksdb_options_set_max_total_wal_size opts . fromIntegral
    c_rocksdb_options_set_create_if_missing
        opts (boolToCBool createIfMissing)
    c_rocksdb_options_set_error_if_exists
        opts (boolToCBool errorIfExists)
    c_rocksdb_options_set_paranoid_checks
        opts (boolToCBool paranoidChecks)
    c_rocksdb_options_set_create_missing_column_families
        opts (boolToCBool createMissingColumnFamilies)
    f opts
  where
    with_opts =
        bracket
        c_rocksdb_options_create
        c_rocksdb_options_destroy


-- | Create options for several column families at once, in the same order as
-- the configurations.
withOptionsCF :: [Config] -> ([Options] -> IO a) -> IO a
withOptionsCF = withOptionsCFCache Nothing

-- | Options of column families that use this block cache, if any.
withOptionsCFCache :: Maybe Cache -> [Config] -> ([Options] -> IO a) -> IO a
withOptionsCFCache cache cfgs f =
    go [] cfgs
  where
    go acc [] = f (reverse acc)
    go acc (c:cs) = withOptionsCache cache c $ \o -> go (o:acc) cs

-- | The block cache of 'blockCacheSize', if it is set. The database and
-- column families that use it keep it alive after this returns.
withBlockCache :: Config -> (Maybe Cache -> IO a) -> IO a
withBlockCache Config {blockCacheSize = Nothing} f = f Nothing
withBlockCache Config {blockCacheSize = Just n} f =
    bracket (c_rocksdb_cache_create_lru (intToCSize n))
            c_rocksdb_cache_destroy
            (f . Just)

-- | Create read options, reading from a snapshot if one is given, and pass them
-- to a function. They are destroyed when it returns.
withReadOpts :: Maybe Snapshot -> (ReadOpts -> IO a) -> IO a
withReadOpts maybe_snap_ptr =
    bracket
    (createReadOpts maybe_snap_ptr)
    c_rocksdb_readoptions_destroy

-- | Create read options without bracket management.
-- Caller is responsible for calling 'destroyReadOpts'.
createReadOpts :: Maybe Snapshot -> IO ReadOpts
createReadOpts maybe_snap_ptr = do
    read_opts_ptr <- c_rocksdb_readoptions_create
    forM_ maybe_snap_ptr $ c_rocksdb_readoptions_set_snapshot read_opts_ptr
    return read_opts_ptr

-- | Destroy read options created with 'createReadOpts'.
destroyReadOpts :: ReadOpts -> IO ()
destroyReadOpts = c_rocksdb_readoptions_destroy

-- | Create default write options and pass them to a function. They are
-- destroyed when it returns.
withWriteOpts :: (WriteOpts -> IO a) -> IO a
withWriteOpts =
    bracket
    c_rocksdb_writeoptions_create
    c_rocksdb_writeoptions_destroy

-- | Free a C string allocated by RocksDB.
freeCString :: CString -> IO ()
freeCString = c_rocksdb_free

-- | Run a C function that takes an error pointer as its last argument. If the
-- function sets an error, free it and throw an 'IOError' whose message starts
-- with the given operation name.
throwIfErr :: String -> (ErrPtr -> IO a) -> IO a
throwIfErr s f = alloca $ \err_ptr -> do
    poke err_ptr nullPtr
    res  <- f err_ptr
    err_cstr <- peek err_ptr
    when (err_cstr /= nullPtr) $ do
        err <- peekCString err_cstr
        free err_cstr
        throwIO $ userError $ s ++ ": " ++ err
    return res

-- | Convert a 'Bool' to a C boolean.
boolToCBool :: Bool -> CBool
boolToCBool True  = 1
boolToCBool False = 0
{-# INLINE boolToCBool #-}

-- | Convert a 'CSize' to an 'Int'.
cSizeToInt :: CSize -> Int
cSizeToInt = fromIntegral
{-# INLINE cSizeToInt #-}

-- | Convert an 'Int' to a 'CSize'.
intToCSize :: Int -> CSize
intToCSize = fromIntegral
{-# INLINE intToCSize #-}

-- | Convert an 'Int' to a 'CInt'.
intToCInt :: Int -> CInt
intToCInt = fromIntegral
{-# INLINE intToCInt #-}

-- | Convert a 'CInt' to an 'Int'.
cIntToInt :: CInt -> Int
cIntToInt = fromIntegral
{-# INLINE cIntToInt #-}

-- | Convert a 'Bool' to @1@ or @0@.
boolToNum :: Num b => Bool -> b
boolToNum True  = fromIntegral (1 :: Int)
boolToNum False = fromIntegral (0 :: Int)
{-# INLINE boolToNum #-}
