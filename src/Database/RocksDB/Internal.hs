{-# LANGUAGE LambdaCase #-}
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
-- Internal types and helpers shared by the other modules. The t'DB' and t'Config'
-- types are re-exported by "Database.RocksDB"; everything else is an
-- implementation detail.
module Database.RocksDB.Internal
  ( Config (..),
    MergeOperator (..),
    DB (..),

    -- * Smart constructors & extractors
    withOptions,
    withOptionsCache,
    withOptionsCF,
    withOptionsCFCache,
    withBlockCache,
    withReadOpts,
    createReadOpts,
    destroyReadOpts,
    withWriteOpts,

    -- * Utilities
    freeCString,
    throwIfErr,
    cSizeToInt,
    intToCSize,
    intToCInt,
    cIntToInt,
    boolToNum,
  )
where

import Control.Exception
import Control.Monad
import Data.Default
import Data.Maybe (isJust)
import Database.RocksDB.C
import Foreign
import Foreign.C.String
import Foreign.C.Types

-- | Handle to an open database. Obtain one with 'Database.RocksDB.withDB' or
-- 'Database.RocksDB.withDBCF'. It is only valid inside the function passed to
-- those, and can be shared between threads.
data DB = DB
  { -- | pointer to the C database object
    rocksDB :: !RocksDB,
    -- | handles of the column families opened with
    -- 'Database.RocksDB.withDBCF', in the order they were given;
    -- empty for 'Database.RocksDB.withDB'
    columnFamilies :: ![ColumnFamily],
    -- | read options used by reads and iterators; set to read from
    -- a snapshot by 'Database.RocksDB.withSnapshot'
    readOpts :: !ReadOpts,
    -- | the snapshot that 'readOpts' read from, if any. RocksDB
    -- cannot be asked what snapshot its read options hold, and
    -- 'Database.RocksDB.withIterConfig' has to make read options
    -- of its own that read from the same one.
    snapshot :: !(Maybe Snapshot),
    -- | write options used by all writes
    writeOpts :: !WriteOpts
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
--
-- Most fields exist to control memory. RocksDB's defaults suit a small database:
-- each column family gets its own cache, the index and filter blocks of every
-- open file stay in memory, and memtables are not capped as a whole. On a large
-- database these grow without bound. Setting 'maxFiles', 'blockCacheSize',
-- 'cacheIndexAndFilterBlocks' and 'dbWriteBufferSize' turns that into a fixed
-- budget.
data Config = Config
  { -- | create the database if it does not exist
    createIfMissing :: !Bool,
    -- | fail to open if the database already exists
    errorIfExists :: !Bool,
    -- | check data aggressively and stop early on any
    -- corruption
    paranoidChecks :: !Bool,
    -- | create the column families passed to
    -- 'Database.RocksDB.withDBCF' that an existing
    -- database does not have yet
    createMissingColumnFamilies :: !Bool,
    -- | maximum number of files RocksDB keeps open
    -- ('Nothing': RocksDB default, unlimited)
    maxFiles :: !(Maybe Int),
    -- | length in bytes of a fixed key prefix, used by
    -- bloom filters and prefix seeks
    prefixLength :: !(Maybe Int),
    -- | use a full bloom filter with 10 bits per key to
    -- skip files that cannot contain a key; the filters
    -- take about 1.25 bytes of memory per key
    bloomFilter :: !Bool,
    -- | bytes per memtable (RocksDB default 64 MiB);
    -- per column family
    writeBufferSize :: !(Maybe Int),
    -- | bytes for all memtables across all column
    -- families (RocksDB default unlimited); database-wide,
    -- ignored in column family configs
    dbWriteBufferSize :: !(Maybe Int),
    -- | bytes of a least-recently-used block cache shared
    -- by the database and all its column families
    -- ('Nothing': RocksDB gives each column family its
    -- own default cache, of 32 MiB in RocksDB 8);
    -- database-wide, ignored in column family configs
    blockCacheSize :: !(Maybe Int),
    -- | keep index and filter blocks in the block cache,
    -- at high priority, with those of level 0 pinned,
    -- instead of in memory outside it for every open
    -- file; per column family
    cacheIndexAndFilterBlocks :: !Bool,
    -- | write the index and the filter of each table file in
    -- pieces of 4 KiB under a small index of their own, where
    -- each is otherwise one block that grows with the file: on
    -- files of 64 MiB the filter is one to three mebibytes and
    -- the index a few hundred kibibytes. A lookup then loads
    -- the two pieces that it needs and not megabytes, and a
    -- block cache far smaller than all the indices and filters
    -- of the database still serves most lookups from memory.
    -- Without it, a database with more of them than its cache
    -- holds loads and drops them at nearly every lookup: a
    -- millisecond of copying, checksums and decompression
    -- each, on a disk that is idle. The small top-level index
    -- of each open file is kept in the cache. It applies to
    -- the files written from then on; the files that a
    -- database already has keep their layout until they are
    -- compacted, and both kinds are read. It costs one more
    -- lookup in memory for each read. Use it with
    -- 'cacheIndexAndFilterBlocks'; per column family
    partitionIndexAndFilters :: !Bool,
    -- | bytes of write-ahead log after which the column
    -- families holding the oldest log are flushed, so it
    -- can be deleted (RocksDB default: four times the
    -- memtables); database-wide, ignored in column family
    -- configs
    maxTotalWalSize :: !(Maybe Int),
    -- | threads that RocksDB may use at once to flush
    -- memtables and to compact files (RocksDB default 2:
    -- one flush and one compaction). Of @n@ jobs, a
    -- quarter are for flushes and the rest for
    -- compactions, at least one of each. More of them
    -- shorten a backlog of compactions on a disk and
    -- cores that have room for it, and take both from
    -- reads when they have not: on a slow disk a single
    -- compaction already waits for the disk, and a
    -- second one only makes every read wait longer.
    -- Database-wide, ignored in column family configs
    maxBackgroundJobs :: !(Maybe Int),
    -- | how RocksDB works out the value of a key that has
    -- merge records ('Database.RocksDB.Merge'): see
    -- t'MergeOperator'. 'Nothing', the default, for a
    -- column family that is never written with merges; one
    -- that is must be given the same operator every time
    -- the database is opened, by every program that opens
    -- it. Per column family: the one in the configuration
    -- of the database is for the default column family
    mergeOperator :: !(Maybe MergeOperator),
    -- | do not compact the files of the column family in
    -- the background. Files then pile up as memtables are
    -- flushed, and reads get slower with each: it is for
    -- a bulk load that ends with
    -- 'Database.RocksDB.compactRange', and for tests and
    -- benchmarks that need to know which file a key is
    -- in. Per column family
    disableAutoCompactions :: !Bool
  }
  deriving (Eq, Show)

-- | How RocksDB combines the merge records of a key with its value.
--
-- = What a merge is
--
-- A put replaces the value of a key, and a program that wants to change
-- a value has to read it first: add to a counter, add an entry to a list.
-- For a batch of thousands of such changes, those reads are most of the
-- work, and each may wait for the disk.
--
-- A /merge record/ is a third kind of write, next to a put and a delete
-- ('Database.RocksDB.Merge', 'Database.RocksDB.merge'). It says how the
-- value changes and not what it becomes: \"add 5\", \"add this entry\".
-- RocksDB stores the record as it is, without reading anything. The work
-- is done later by the /merge operator/ of the column family: a function
-- that takes the value that the key had and the records written since,
-- oldest first, and gives the value that the key now has. RocksDB calls
-- it when the key is read, and when a compaction brings the records and
-- the value together, after which the key holds a plain value again.
--
-- Readers see none of this: 'Database.RocksDB.get',
-- 'Database.RocksDB.multiGet' and iterators return merged values. What
-- they pay is the merge, for a key that has records pending, and a look
-- into each file that holds one of them.
--
-- A put or a delete after merge records hides them, like any older
-- value. A merge record after a delete, or on a key that was never
-- written, is merged with no value: the operator decides what that means.
--
-- = Which operator
--
-- The operator is part of the meaning of the data, like the format of
-- the values. Every program that opens the database must give the column
-- family the same operator, because RocksDB does not refuse to open one
-- that has merge records without it. This is what RocksDB 11.1 was seen to
-- do then:
--
-- * Merge records that are still in the write-ahead log, not yet flushed
--   to a table file, are __dropped when the database is opened__, without
--   an error. They are lost for good.
--
-- * Merge records in table files are kept, also through a compaction, and
--   are there again for a program that opens the database with the
--   operator. Meanwhile they cannot be read: 'Database.RocksDB.get' throws
--   for a key that has records and no value under them, an iterator stops
--   at such a key with an error, and a 'Database.RocksDB.get' or
--   'Database.RocksDB.multiGet' of a key that has a value under its
--   records __crashes the process__.
--
-- * Writing a merge record is refused.
--
-- A column family opened with another operator gives whatever that
-- operator makes of the records. So a program that starts to write merge
-- records to a database that older versions of it can open should make
-- sure that they no longer can: a column family that they do not know of
-- is enough, as RocksDB refuses to open a database without all its column
-- families, before it reads the write-ahead log.
data MergeOperator
  = -- | RocksDB's own operator for counters. A value and a merge record
    -- are both an unsigned 64-bit number in eight bytes, least significant
    -- byte first, and merging adds them, modulo 2^64. A key without a
    -- value counts from zero, and so does one whose value is not eight
    -- bytes long.
    UInt64Add
  | -- | An operator made of two C functions.
    --
    -- They are C functions and not Haskell ones because RocksDB calls
    -- them from its own threads, during reads, flushes and compactions,
    -- several at once. Import their addresses:
    --
    -- @
    -- foreign import ccall "&my_full_merge" myFullMerge :: FunPtr FullMerge
    -- foreign import ccall "&my_partial_merge" myPartialMerge :: FunPtr PartialMerge
    -- @
    --
    -- @cbits\/rocksdb_haskell.h@ in this package has their C types, as
    -- @rocksdb_haskell_full_merge_t@ and @rocksdb_haskell_partial_merge_t@,
    -- and what they must do: return the result in memory from @malloc@,
    -- which RocksDB frees, and how to report a failure.
    MergeFunctions
      { -- | the name of the operator, which RocksDB writes in its
        -- options file and in every table file. Change it when what
        -- the functions do changes.
        mergeName :: !String,
        -- | the value of a key from the value it had, if any, and its
        -- merge records, oldest first
        fullMerge :: !(FunPtr FullMerge),
        -- | one merge record from several, oldest first, that does what
        -- they did. RocksDB uses it to shorten the list where it has
        -- records and not the value under them. A function that always
        -- fails is allowed: the records are then kept as they are.
        partialMerge :: !(FunPtr PartialMerge)
      }
  deriving (Eq, Show)

-- | All flags off and all sizes left to RocksDB defaults.
instance Default Config where
  def =
    Config
      { createIfMissing = False,
        errorIfExists = False,
        paranoidChecks = False,
        createMissingColumnFamilies = False,
        maxFiles = Nothing,
        prefixLength = Nothing,
        bloomFilter = False,
        writeBufferSize = Nothing,
        dbWriteBufferSize = Nothing,
        blockCacheSize = Nothing,
        cacheIndexAndFilterBlocks = False,
        partitionIndexAndFilters = False,
        maxTotalWalSize = Nothing,
        maxBackgroundJobs = Nothing,
        mergeOperator = Nothing,
        disableAutoCompactions = False
      }

-- | @rocksdb_block_based_table_index_type_two_level_index_search@: an index
-- of the pieces of the index.
twoLevelIndexSearch :: CInt
twoLevelIndexSearch = 2

-- | Create RocksDB options from a t'Config' and pass them to a function. The
-- options are destroyed when it returns.
withOptions :: Config -> (Options -> IO a) -> IO a
withOptions = withOptionsCache Nothing

-- | Options of a database or column family that use this block cache, if
-- any. The options are destroyed when the function returns.
withOptionsCache :: Maybe Cache -> Config -> (Options -> IO a) -> IO a
withOptionsCache cache Config {..} f = with_opts $ \opts -> do
  when (bloomFilter || isJust cache || cacheIndexAndFilterBlocks || partitionIndexAndFilters)
    $ bracket
      c_rocksdb_block_based_options_create
      c_rocksdb_block_based_options_destroy
    $ \bo -> do
      when bloomFilter $ do
        fp <- c_rocksdb_filterpolicy_create_bloom_full 10
        c_rocksdb_block_based_options_set_filter_policy bo fp
      forM_ cache $ c_rocksdb_block_based_options_set_block_cache bo
      when cacheIndexAndFilterBlocks $ do
        c_rocksdb_block_based_options_set_cache_index_and_filter_blocks bo 1
        c_rocksdb_block_based_options_set_cache_index_and_filter_blocks_with_high_priority bo 1
        c_rocksdb_block_based_options_set_pin_l0_filter_and_index_blocks_in_cache bo 1
      when partitionIndexAndFilters $ do
        c_rocksdb_block_based_options_set_index_type bo twoLevelIndexSearch
        c_rocksdb_block_based_options_set_partition_filters bo 1
        c_rocksdb_block_based_options_set_metadata_block_size bo 4096
        c_rocksdb_block_based_options_set_pin_top_level_index_and_filter bo 1
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
  forM_ maxBackgroundJobs $
    c_rocksdb_options_set_max_background_jobs opts . intToCInt
  forM_ mergeOperator $ \case
    UInt64Add -> c_rocksdb_options_set_uint64add_merge_operator opts
    MergeFunctions {..} ->
      withCString mergeName $ \name -> do
        status <- c_rocksdb_haskell_options_set_merge_operator opts name fullMerge partialMerge
        when (status /= 0) $
          throwIO (userError "mergeOperator: no memory for the merge operator")
  when disableAutoCompactions $
    c_rocksdb_options_set_disable_auto_compactions opts 1
  c_rocksdb_options_set_create_if_missing
    opts
    (boolToCBool createIfMissing)
  c_rocksdb_options_set_error_if_exists
    opts
    (boolToCBool errorIfExists)
  c_rocksdb_options_set_paranoid_checks
    opts
    (boolToCBool paranoidChecks)
  c_rocksdb_options_set_create_missing_column_families
    opts
    (boolToCBool createMissingColumnFamilies)
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
    go acc (c : cs) = withOptionsCache cache c $ \o -> go (o : acc) cs

-- | The block cache of 'blockCacheSize', if it is set. The database and
-- column families that use it keep it alive after this returns.
--
-- A single cache for the whole database lets RocksDB give memory to whichever
-- column family is busy, instead of fixing a share for each.
withBlockCache :: Config -> (Maybe Cache -> IO a) -> IO a
withBlockCache Config {blockCacheSize = Nothing} f = f Nothing
withBlockCache Config {blockCacheSize = Just n} f =
  bracket
    (c_rocksdb_cache_create_lru (intToCSize n))
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
--
-- Every C function that can fail reports it this way, so this is the one place
-- where RocksDB errors become Haskell exceptions, and where their messages are
-- freed.
throwIfErr :: String -> (ErrPtr -> IO a) -> IO a
throwIfErr s f = alloca $ \err_ptr -> do
  poke err_ptr nullPtr
  res <- f err_ptr
  err_cstr <- peek err_ptr
  when (err_cstr /= nullPtr) $ do
    err <- peekCString err_cstr
    free err_cstr
    throwIO $ userError $ s ++ ": " ++ err
  return res

-- | Convert a 'Bool' to a C boolean.
boolToCBool :: Bool -> CBool
boolToCBool True = 1
boolToCBool False = 0
{-# INLINE boolToCBool #-}

-- | Convert a t'CSize' to an 'Int'.
cSizeToInt :: CSize -> Int
cSizeToInt = fromIntegral
{-# INLINE cSizeToInt #-}

-- | Convert an 'Int' to a t'CSize'.
intToCSize :: Int -> CSize
intToCSize = fromIntegral
{-# INLINE intToCSize #-}

-- | Convert an 'Int' to a t'CInt'.
intToCInt :: Int -> CInt
intToCInt = fromIntegral
{-# INLINE intToCInt #-}

-- | Convert a t'CInt' to an 'Int'.
cIntToInt :: CInt -> Int
cIntToInt = fromIntegral
{-# INLINE cIntToInt #-}

-- | Convert a 'Bool' to @1@ or @0@.
boolToNum :: (Num b) => Bool -> b
boolToNum True = fromIntegral (1 :: Int)
boolToNum False = fromIntegral (0 :: Int)
{-# INLINE boolToNum #-}
