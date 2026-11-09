{-# LANGUAGE CApiFFI                  #-}
{-# LANGUAGE CPP                      #-}
{-# LANGUAGE EmptyDataDecls           #-}
{-# LANGUAGE ForeignFunctionInterface #-}
-- |
-- Module      : Database.RocksDB.C
-- Copyright   : (c) 2012-2020 The rocksdb-haskell Authors
-- License     : BSD3
-- Maintainer  : jprupp@protonmail.ch
-- Stability   : experimental
-- Portability : non-portable
--
-- Raw FFI bindings to the RocksDB C API declared in @rocksdb/c.h@. This module
-- is internal; use "Database.RocksDB" instead.
--
-- Each @c_rocksdb_*@ function is a direct binding to the C function of the same
-- name without the @c_@ prefix. Functions that take an 'ErrPtr' report failure
-- by storing a @malloc@-allocated error message in it, which the caller must
-- free. See 'Database.RocksDB.Internal.throwIfErr'.
--
-- The bindings are @safe@ foreign calls. RocksDB calls can take a long time,
-- for example when they wait for disk or for a compaction to make room, and a
-- @safe@ call lets other Haskell threads keep running meanwhile. The small
-- extra cost per call is worth that in a concurrent program.
--
-- = Keeping the types in step with the header
--
-- The compiler does not check a foreign import against @rocksdb\/c.h@: it
-- takes the Haskell type on trust. An argument of the wrong kind, or a
-- missing one, compiles, links and runs, and RocksDB then works with garbage
-- in its place. RocksDB has changed the types of functions that already
-- existed, twice among the ones bound here: the error pointer added to
-- @rocksdb_approximate_sizes@ in 6.16, and the bits per key of the Bloom
-- filter policies, an @int@ until 6.24 and a @double@ since. When adding or
-- reviewing a binding, read its prototype in the header, argument by
-- argument. Where a type differs between versions of RocksDB, import the
-- function with @capi@ instead of @ccall@, as
-- 'c_rocksdb_filterpolicy_create_bloom_full' is: the C compiler then reads
-- the header that is installed, and converts the argument.

module Database.RocksDB.C
    ( RocksDB
    , Options
    , ReadOpts
    , WriteOpts
    , ColumnFamily
    , SliceTransform
    , Snapshot
    , Iterator
    , WriteBatch
    , FilterPolicy
    , Cache
    , ErrPtr
    , DBName
    , CFName
    , Key
    , Val
    , c_rocksdb_open
    , c_rocksdb_open_column_families
    , c_rocksdb_close
    , c_rocksdb_create_column_family
    , c_rocksdb_drop_column_family
    , c_rocksdb_column_family_handle_destroy
    , c_rocksdb_put
    , c_rocksdb_put_cf
    , c_rocksdb_delete
    , c_rocksdb_delete_cf
    , c_rocksdb_write
    , c_rocksdb_get
    , c_rocksdb_get_cf
    , c_rocksdb_multi_get
    , c_rocksdb_multi_get_cf
    , c_rocksdb_create_snapshot
    , c_rocksdb_release_snapshot
    , c_rocksdb_property_value
    , c_rocksdb_property_value_cf
    , c_rocksdb_approximate_sizes
    , c_rocksdb_destroy_db
    , c_rocksdb_repair_db
    , c_rocksdb_create_iterator
    , c_rocksdb_create_iterator_cf
    , c_rocksdb_iter_destroy
    , c_rocksdb_iter_valid
    , c_rocksdb_iter_seek_to_first
    , c_rocksdb_iter_seek_to_last
    , c_rocksdb_iter_seek
    , c_rocksdb_iter_next
    , c_rocksdb_iter_prev
    , c_rocksdb_iter_key
    , c_rocksdb_iter_value
    , c_rocksdb_iter_get_error
    , c_rocksdb_writebatch_create
    , c_rocksdb_writebatch_destroy
    , c_rocksdb_writebatch_clear
    , c_rocksdb_writebatch_put
    , c_rocksdb_writebatch_put_cf
    , c_rocksdb_writebatch_delete
    , c_rocksdb_writebatch_delete_cf
    , c_rocksdb_options_create
    , c_rocksdb_options_destroy
    , c_rocksdb_options_set_create_if_missing
    , c_rocksdb_options_set_error_if_exists
    , c_rocksdb_options_set_paranoid_checks
    , c_rocksdb_options_set_max_open_files
    , c_rocksdb_options_set_write_buffer_size
    , c_rocksdb_options_set_db_write_buffer_size
    , c_rocksdb_options_set_max_total_wal_size
    , c_rocksdb_options_set_create_missing_column_families
    , c_rocksdb_options_set_prefix_extractor
    , c_rocksdb_options_set_block_based_table_factory
    , c_rocksdb_block_based_options_create
    , c_rocksdb_block_based_options_set_filter_policy
    , c_rocksdb_block_based_options_destroy
    , c_rocksdb_block_based_options_set_block_cache
    , c_rocksdb_block_based_options_set_cache_index_and_filter_blocks
    , c_rocksdb_block_based_options_set_cache_index_and_filter_blocks_with_high_priority
    , c_rocksdb_block_based_options_set_pin_l0_filter_and_index_blocks_in_cache
    , c_rocksdb_cache_create_lru
    , c_rocksdb_cache_destroy
    , c_rocksdb_slicetransform_create_fixed_prefix
    , c_rocksdb_slicetransform_destroy
    , c_rocksdb_filterpolicy_destroy
    , c_rocksdb_filterpolicy_create_bloom_full
    , c_rocksdb_readoptions_create
    , c_rocksdb_readoptions_destroy
    , c_rocksdb_readoptions_set_snapshot
    , c_rocksdb_writeoptions_create
    , c_rocksdb_writeoptions_destroy
    , c_rocksdb_free
    ) where

import           Foreign
import           Foreign.C.String
import           Foreign.C.Types

-- | Opaque C database object.
data LRocksDB
-- | Opaque C column family handle.
data LColumnFamily
-- | Opaque C iterator.
data LIterator
-- | Opaque C database or column family options.
data LOptions
-- | Opaque C block-based table options.
data LBlockBasedOptions
-- | Opaque C read options.
data LReadOpts
-- | Opaque C snapshot.
data LSnapshot
-- | Opaque C write batch.
data LWriteBatch
-- | Opaque C write options.
data LWriteOpts
-- | Opaque C filter policy.
data LFilterPolicy
-- | Opaque C slice transform (prefix extractor).
data LSliceTransform
-- | Opaque C block cache.
data LCache

-- | Pointer to an open database.
type RocksDB            = Ptr LRocksDB
-- | Pointer to a column family handle.
type ColumnFamily       = Ptr LColumnFamily
-- | Pointer to database or column family options.
type Options            = Ptr LOptions
-- | Pointer to a batch of writes applied atomically.
type WriteBatch         = Ptr LWriteBatch
-- | Pointer to a key transformation, used to extract prefixes.
type SliceTransform     = Ptr LSliceTransform
-- | Pointer to options for reads and iterators.
type ReadOpts           = Ptr LReadOpts
-- | Pointer to options for writes.
type WriteOpts          = Ptr LWriteOpts
-- | Pointer to a consistent point-in-time view of a database.
type Snapshot           = Ptr LSnapshot
-- | Pointer to an iterator over keys.
type Iterator           = Ptr LIterator
-- | Pointer to a filter policy, such as a bloom filter.
type FilterPolicy       = Ptr LFilterPolicy
-- | Pointer to block-based table options.
type BlockBasedOptions  = Ptr LBlockBasedOptions
-- | Pointer to a block cache.
type Cache              = Ptr LCache

-- | Pointer to a location where a C function stores an error message, or leaves @NULL@ on success.
type ErrPtr           = Ptr CString
-- | Path of a database, as a C string.
type DBName           = CString
-- | Name of a column family, as a C string.
type CFName           = CString
-- | Pointer to the bytes of a key. Its length is passed separately.
type Key              = CString
-- | Pointer to the bytes of a value. Its length is passed separately.
type Val              = CString

-- | Binding to @rocksdb_open@.
foreign import ccall safe "rocksdb/c.h rocksdb_open"
  c_rocksdb_open :: Options -> DBName -> ErrPtr -> IO RocksDB

-- | Binding to @rocksdb_open_column_families@.
foreign import ccall safe "rocksdb/c.h rocksdb_open_column_families"
  c_rocksdb_open_column_families :: Options
                                 -> DBName
                                 -> CInt
                                 -> Ptr CFName
                                 -> Ptr Options
                                 -> Ptr ColumnFamily
                                 -> ErrPtr
                                 -> IO RocksDB

-- | Binding to @rocksdb_close@.
foreign import ccall safe "rocksdb/c.h rocksdb_close"
  c_rocksdb_close :: RocksDB -> IO ()

-- | Binding to @rocksdb_create_column_family@.
foreign import ccall safe "rocksdb/c.h rocksdb_create_column_family"
  c_rocksdb_create_column_family :: RocksDB
                                 -> Options
                                 -> CFName
                                 -> ErrPtr
                                 -> IO ColumnFamily

-- | Binding to @rocksdb_drop_column_family@.
foreign import ccall safe "rocksdb/c.h rocksdb_drop_column_family"
  c_rocksdb_drop_column_family :: RocksDB
                               -> ColumnFamily
                               -> ErrPtr
                               -> IO ()

-- | Binding to @rocksdb_column_family_handle_destroy@.
foreign import ccall safe "rocksdb/c.h rocksdb_column_family_handle_destroy"
  c_rocksdb_column_family_handle_destroy :: ColumnFamily -> IO ()

-- | Binding to @rocksdb_put@.
foreign import ccall safe "rocksdb/c.h rocksdb_put"
  c_rocksdb_put :: RocksDB
                -> WriteOpts
                -> Key -> CSize
                -> Val -> CSize
                -> ErrPtr
                -> IO ()

-- | Binding to @rocksdb_put_cf@.
foreign import ccall safe "rocksdb/c.h rocksdb_put_cf"
  c_rocksdb_put_cf :: RocksDB
                   -> WriteOpts
                   -> ColumnFamily
                   -> Key -> CSize
                   -> Val -> CSize
                   -> ErrPtr
                   -> IO ()

-- | Binding to @rocksdb_delete@.
foreign import ccall safe "rocksdb/c.h rocksdb_delete"
  c_rocksdb_delete :: RocksDB
                   -> WriteOpts
                   -> Key -> CSize
                   -> ErrPtr
                   -> IO ()

-- | Binding to @rocksdb_delete_cf@.
foreign import ccall safe "rocksdb/c.h rocksdb_delete_cf"
  c_rocksdb_delete_cf :: RocksDB
                      -> WriteOpts
                      -> ColumnFamily
                      -> Key -> CSize
                      -> ErrPtr
                      -> IO ()

-- | Binding to @rocksdb_write@.
foreign import ccall safe "rocksdb/c.h rocksdb_write"
  c_rocksdb_write :: RocksDB
                  -> WriteOpts
                  -> WriteBatch
                  -> ErrPtr
                  -> IO ()

-- | Returns NULL if not found. A malloc()ed array otherwise.
-- Stores the length of the array in *vallen.
--
-- Binding to @rocksdb_get@.
foreign import ccall safe "rocksdb/c.h rocksdb_get"
  c_rocksdb_get :: RocksDB
                -> ReadOpts
                -> Key -> CSize
                -> Ptr CSize        -- ^ value length
                -> ErrPtr
                -> IO CString

-- | Binding to @rocksdb_get_cf@.
foreign import ccall safe "rocksdb/c.h rocksdb_get_cf"
  c_rocksdb_get_cf :: RocksDB
                   -> ReadOpts
                   -> ColumnFamily
                   -> Key -> CSize
                   -> Ptr CSize        -- ^ value length
                   -> ErrPtr
                   -> IO CString

-- | Binding to @rocksdb_multi_get@.
foreign import ccall safe "rocksdb/c.h rocksdb_multi_get"
  c_rocksdb_multi_get :: RocksDB
                      -> ReadOpts
                      -> CSize        -- ^ number of keys
                      -> Ptr CString  -- ^ keys
                      -> Ptr CSize    -- ^ key sizes
                      -> Ptr CString  -- ^ values
                      -> Ptr CSize    -- ^ values sizes
                      -> ErrPtr
                      -> IO ()

-- | Binding to @rocksdb_multi_get_cf@.
foreign import ccall safe "rocksdb/c.h rocksdb_multi_get_cf"
  c_rocksdb_multi_get_cf :: RocksDB
                         -> ReadOpts
                         -> Ptr ColumnFamily -- ^ column families
                         -> CSize                   -- ^ number of keys
                         -> Ptr CString             -- ^ keys
                         -> Ptr CSize               -- ^ key sizes
                         -> Ptr CString             -- ^ values
                         -> Ptr CSize               -- ^ values sizes
                         -> ErrPtr
                         -> IO ()

-- | Binding to @rocksdb_create_snapshot@.
foreign import ccall safe "rocksdb/c.h rocksdb_create_snapshot"
  c_rocksdb_create_snapshot :: RocksDB -> IO Snapshot

-- | Binding to @rocksdb_release_snapshot@.
foreign import ccall safe "rocksdb/c.h rocksdb_release_snapshot"
  c_rocksdb_release_snapshot :: RocksDB -> Snapshot -> IO ()

-- | Returns NULL if property name is unknown. Else returns a pointer to a
-- malloc()-ed null-terminated value.
--
-- Binding to @rocksdb_property_value@.
foreign import ccall safe "rocksdb/c.h rocksdb_property_value"
  c_rocksdb_property_value :: RocksDB -> CString -> IO CString

-- | Binding to @rocksdb_property_value_cf@.
foreign import ccall safe "rocksdb/c.h rocksdb_property_value_cf"
  c_rocksdb_property_value_cf :: RocksDB -> ColumnFamily -> CString -> IO CString

-- | Binding to @rocksdb_approximate_sizes@.
--
-- The last argument receives an error, as in every function that can fail.
-- RocksDB added it in 6.16. A binding without it leaves the C function to
-- read a pointer that was never passed, and to write through it on failure.
-- Older versions do not take the argument, and ignore it.
foreign import ccall safe "rocksdb/c.h rocksdb_approximate_sizes"
  c_rocksdb_approximate_sizes :: RocksDB
                              -> CInt
                                 -- ^ num ranges
                              -> Ptr CString -> Ptr CSize
                                 -- ^ range start keys (array)
                              -> Ptr CString -> Ptr CSize
                                 -- ^ range limit keys (array)
                              -> Ptr Word64
                                 -- ^ array of approx. sizes of ranges
                              -> ErrPtr
                              -> IO ()

-- | Binding to @rocksdb_destroy_db@.
foreign import ccall safe "rocksdb/c.h rocksdb_destroy_db"
  c_rocksdb_destroy_db :: Options -> DBName -> ErrPtr -> IO ()

-- | Binding to @rocksdb_repair_db@.
foreign import ccall safe "rocksdb/c.h rocksdb_repair_db"
  c_rocksdb_repair_db :: Options -> DBName -> ErrPtr -> IO ()


--
-- Iterator
--

-- | Binding to @rocksdb_create_iterator@.
foreign import ccall safe "rocksdb/c.h rocksdb_create_iterator"
  c_rocksdb_create_iterator :: RocksDB
                            -> ReadOpts
                            -> IO Iterator

-- | Binding to @rocksdb_create_iterator_cf@.
foreign import ccall safe "rocksdb/c.h rocksdb_create_iterator_cf"
  c_rocksdb_create_iterator_cf :: RocksDB
                               -> ReadOpts
                               -> ColumnFamily
                               -> IO Iterator

-- | Binding to @rocksdb_iter_destroy@.
foreign import ccall safe "rocksdb/c.h rocksdb_iter_destroy"
  c_rocksdb_iter_destroy :: Iterator -> IO ()

-- | Binding to @rocksdb_iter_valid@.
foreign import ccall safe "rocksdb/c.h rocksdb_iter_valid"
  c_rocksdb_iter_valid :: Iterator -> IO CUChar

-- | Binding to @rocksdb_iter_seek_to_first@.
foreign import ccall safe "rocksdb/c.h rocksdb_iter_seek_to_first"
  c_rocksdb_iter_seek_to_first :: Iterator -> IO ()

-- | Binding to @rocksdb_iter_seek_to_last@.
foreign import ccall safe "rocksdb/c.h rocksdb_iter_seek_to_last"
  c_rocksdb_iter_seek_to_last :: Iterator -> IO ()

-- | Binding to @rocksdb_iter_seek@.
foreign import ccall safe "rocksdb/c.h rocksdb_iter_seek"
  c_rocksdb_iter_seek :: Iterator -> Key -> CSize -> IO ()

-- | Binding to @rocksdb_iter_next@.
foreign import ccall safe "rocksdb/c.h rocksdb_iter_next"
  c_rocksdb_iter_next :: Iterator -> IO ()

-- | Binding to @rocksdb_iter_prev@.
foreign import ccall safe "rocksdb/c.h rocksdb_iter_prev"
  c_rocksdb_iter_prev :: Iterator -> IO ()

-- | Binding to @rocksdb_iter_key@.
foreign import ccall safe "rocksdb/c.h rocksdb_iter_key"
  c_rocksdb_iter_key :: Iterator -> Ptr CSize -> IO Key

-- | Binding to @rocksdb_iter_value@.
foreign import ccall safe "rocksdb/c.h rocksdb_iter_value"
  c_rocksdb_iter_value :: Iterator -> Ptr CSize -> IO Val

-- | Binding to @rocksdb_iter_get_error@.
foreign import ccall safe "rocksdb/c.h rocksdb_iter_get_error"
  c_rocksdb_iter_get_error :: Iterator -> ErrPtr -> IO ()


--
-- Write batch
--

-- | Binding to @rocksdb_writebatch_create@.
foreign import ccall safe "rocksdb/c.h rocksdb_writebatch_create"
  c_rocksdb_writebatch_create :: IO WriteBatch

-- | Binding to @rocksdb_writebatch_destroy@.
foreign import ccall safe "rocksdb/c.h rocksdb_writebatch_destroy"
  c_rocksdb_writebatch_destroy :: WriteBatch -> IO ()

-- | Binding to @rocksdb_writebatch_clear@.
foreign import ccall safe "rocksdb/c.h rocksdb_writebatch_clear"
  c_rocksdb_writebatch_clear :: WriteBatch -> IO ()

-- | Binding to @rocksdb_writebatch_put@.
foreign import ccall safe "rocksdb/c.h rocksdb_writebatch_put"
  c_rocksdb_writebatch_put :: WriteBatch
                           -> Key -> CSize
                           -> Val -> CSize
                           -> IO ()

-- | Binding to @rocksdb_writebatch_put_cf@.
foreign import ccall safe "rocksdb/c.h rocksdb_writebatch_put_cf"
  c_rocksdb_writebatch_put_cf :: WriteBatch
                              -> ColumnFamily
                              -> Key -> CSize
                              -> Val -> CSize
                              -> IO ()

-- | Binding to @rocksdb_writebatch_delete@.
foreign import ccall safe "rocksdb/c.h rocksdb_writebatch_delete"
  c_rocksdb_writebatch_delete :: WriteBatch
                              -> Key -> CSize -> IO ()

-- | Binding to @rocksdb_writebatch_delete_cf@.
foreign import ccall safe "rocksdb/c.h rocksdb_writebatch_delete_cf"
  c_rocksdb_writebatch_delete_cf :: WriteBatch
                                 -> ColumnFamily
                                 -> Key
                                 -> CSize
                                 -> IO ()

--
-- Options
--

-- | Binding to @rocksdb_options_create@.
foreign import ccall safe "rocksdb/c.h rocksdb_options_create"
  c_rocksdb_options_create :: IO Options

-- | Binding to @rocksdb_options_destroy@.
foreign import ccall "rocksdb/c.h rocksdb_options_destroy"
  c_rocksdb_options_destroy :: Options -> IO ()

-- | Binding to @rocksdb_options_set_create_if_missing@.
foreign import ccall safe "rocksdb/c.h rocksdb_options_set_create_if_missing"
  c_rocksdb_options_set_create_if_missing :: Options -> CBool -> IO ()

-- | Binding to @rocksdb_options_set_error_if_exists@.
foreign import ccall safe "rocksdb/c.h rocksdb_options_set_error_if_exists"
  c_rocksdb_options_set_error_if_exists :: Options -> CBool -> IO ()

-- | Binding to @rocksdb_options_set_paranoid_checks@.
foreign import ccall safe "rocksdb/c.h rocksdb_options_set_paranoid_checks"
  c_rocksdb_options_set_paranoid_checks :: Options -> CBool -> IO ()

-- | Binding to @rocksdb_options_set_max_open_files@.
foreign import ccall safe "rocksdb/c.h rocksdb_options_set_max_open_files"
  c_rocksdb_options_set_max_open_files :: Options -> CInt -> IO ()

-- | Binding to @rocksdb_options_set_write_buffer_size@.
foreign import ccall safe "rocksdb/c.h rocksdb_options_set_write_buffer_size"
  c_rocksdb_options_set_write_buffer_size :: Options -> CSize -> IO ()

-- | Binding to @rocksdb_options_set_db_write_buffer_size@.
foreign import ccall safe "rocksdb/c.h rocksdb_options_set_db_write_buffer_size"
  c_rocksdb_options_set_db_write_buffer_size :: Options -> CSize -> IO ()

-- | Binding to @rocksdb_options_set_max_total_wal_size@.
foreign import ccall safe "rocksdb/c.h rocksdb_options_set_max_total_wal_size"
  c_rocksdb_options_set_max_total_wal_size :: Options -> Word64 -> IO ()

-- | Binding to @rocksdb_options_set_create_missing_column_families@.
foreign import ccall safe "rocksdb/c.h rocksdb_options_set_create_missing_column_families"
  c_rocksdb_options_set_create_missing_column_families :: Options -> CBool -> IO ()

-- | Binding to @rocksdb_options_set_prefix_extractor@.
foreign import ccall safe "rocksdb/c.h rocksdb_options_set_prefix_extractor"
  c_rocksdb_options_set_prefix_extractor :: Options
                                         -> SliceTransform
                                         -> IO ()

--
-- Block-Based Options
--

-- | Binding to @rocksdb_block_based_options_create@.
foreign import ccall safe "rocksdb/c.h rocksdb_block_based_options_create"
  c_rocksdb_block_based_options_create :: IO BlockBasedOptions

-- | Binding to @rocksdb_block_based_options_destroy@.
foreign import ccall safe "rocksdb/c.h rocksdb_block_based_options_destroy"
  c_rocksdb_block_based_options_destroy :: BlockBasedOptions
                                        -> IO ()

-- | Binding to @rocksdb_block_based_options_set_filter_policy@.
foreign import ccall safe "rocksdb/c.h rocksdb_block_based_options_set_filter_policy"
  c_rocksdb_block_based_options_set_filter_policy :: BlockBasedOptions
                                                  -> FilterPolicy
                                                  -> IO ()

-- | Binding to @rocksdb_block_based_options_set_block_cache@.
foreign import ccall safe "rocksdb/c.h rocksdb_block_based_options_set_block_cache"
  c_rocksdb_block_based_options_set_block_cache :: BlockBasedOptions
                                                -> Cache
                                                -> IO ()

-- | Binding to @rocksdb_block_based_options_set_cache_index_and_filter_blocks@.
foreign import ccall safe "rocksdb/c.h rocksdb_block_based_options_set_cache_index_and_filter_blocks"
  c_rocksdb_block_based_options_set_cache_index_and_filter_blocks :: BlockBasedOptions
                                                                  -> CUChar
                                                                  -> IO ()

-- | Binding to @rocksdb_block_based_options_set_cache_index_and_filter_blocks_with_high_priority@.
foreign import ccall safe
  "rocksdb/c.h rocksdb_block_based_options_set_cache_index_and_filter_blocks_with_high_priority"
  c_rocksdb_block_based_options_set_cache_index_and_filter_blocks_with_high_priority :: BlockBasedOptions
                                                                                     -> CUChar
                                                                                     -> IO ()

-- | Binding to @rocksdb_block_based_options_set_pin_l0_filter_and_index_blocks_in_cache@.
foreign import ccall safe "rocksdb/c.h rocksdb_block_based_options_set_pin_l0_filter_and_index_blocks_in_cache"
  c_rocksdb_block_based_options_set_pin_l0_filter_and_index_blocks_in_cache :: BlockBasedOptions
                                                                            -> CUChar
                                                                            -> IO ()

-- | Binding to @rocksdb_options_set_block_based_table_factory@.
foreign import ccall safe "rocksdb/c.h rocksdb_options_set_block_based_table_factory"
  c_rocksdb_options_set_block_based_table_factory :: Options
                                                  -> BlockBasedOptions
                                                  -> IO ()

--
-- Block Cache
--

-- | Binding to @rocksdb_cache_create_lru@.
foreign import ccall safe "rocksdb/c.h rocksdb_cache_create_lru"
  c_rocksdb_cache_create_lru :: CSize -> IO Cache

-- | Binding to @rocksdb_cache_destroy@.
foreign import ccall safe "rocksdb/c.h rocksdb_cache_destroy"
  c_rocksdb_cache_destroy :: Cache -> IO ()

--
-- Prefix Extractor
--

-- | Binding to @rocksdb_slicetransform_create_fixed_prefix@.
foreign import ccall safe "rocksdb/c.h rocksdb_slicetransform_create_fixed_prefix"
  c_rocksdb_slicetransform_create_fixed_prefix :: CSize -> IO SliceTransform

-- | Binding to @rocksdb_slicetransform_destroy@.
foreign import ccall safe "rocksdb/c.h rocksdb_slicetransform_destroy"
  c_rocksdb_slicetransform_destroy :: SliceTransform -> IO ()

--
-- Bloom Filter
--

-- | Binding to @rocksdb_filterpolicy_destroy@.
foreign import ccall safe "rocksdb/c.h rocksdb_filterpolicy_destroy"
  c_rocksdb_filterpolicy_destroy :: FilterPolicy -> IO ()

-- | Binding to @rocksdb_filterpolicy_create_bloom_full@.
--
-- The bits per key are a @double@ in C since RocksDB 6.24, and were an @int@
-- before. Passed as the wrong one of the two, they travel in the wrong
-- register: the function reads whatever the other register holds, takes it
-- for the bits per key, and with too few of them builds no filter at all.
-- Nothing fails; lookups of absent keys are just as slow as without a filter.
--
-- So this import alone uses the @capi@ calling convention. GHC then calls the
-- function through a stub that it compiles against @rocksdb\/c.h@, and the C
-- compiler converts the argument to the type that the installed RocksDB
-- declares, whichever it is.
foreign import capi safe "rocksdb/c.h rocksdb_filterpolicy_create_bloom_full"
  c_rocksdb_filterpolicy_create_bloom_full :: CDouble -> IO FilterPolicy

--
-- Read options
--

-- | Binding to @rocksdb_readoptions_create@.
foreign import ccall safe "rocksdb/c.h rocksdb_readoptions_create"
  c_rocksdb_readoptions_create :: IO ReadOpts

-- | Binding to @rocksdb_readoptions_destroy@.
foreign import ccall safe "rocksdb/c.h rocksdb_readoptions_destroy"
  c_rocksdb_readoptions_destroy :: ReadOpts -> IO ()

-- | Binding to @rocksdb_readoptions_set_snapshot@.
foreign import ccall safe "rocksdb/c.h rocksdb_readoptions_set_snapshot"
  c_rocksdb_readoptions_set_snapshot :: ReadOpts -> Snapshot -> IO ()


--
-- Write options
--

-- | Binding to @rocksdb_writeoptions_create@.
foreign import ccall safe "rocksdb/c.h rocksdb_writeoptions_create"
  c_rocksdb_writeoptions_create :: IO WriteOpts

-- | Binding to @rocksdb_writeoptions_destroy@.
foreign import ccall safe "rocksdb/c.h rocksdb_writeoptions_destroy"
  c_rocksdb_writeoptions_destroy :: WriteOpts -> IO ()

--
-- Free
--

-- | Binding to @rocksdb_free@.
foreign import ccall safe "rocksdb/c.h rocksdb_free"
  c_rocksdb_free :: CString -> IO ()
