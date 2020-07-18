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

module Database.RocksDB.C where

import           Foreign
import           Foreign.C.String
import           Foreign.C.Types

data LRocksDB
data LColumnFamily
data LIterator
data LOptions
data LReadOpts
data LSnapshot
data LWriteBatch
data LWriteOpts
data LBloomFilter
data LPrefixExtract

type DBName = CString
type CFName = CString
type ErrPtr = Ptr CString
type Key    = CString
type Val    = CString

foreign import ccall safe "rocksdb/c.h rocksdb_open"
  c_rocksdb_open :: Ptr LOptions -> DBName -> ErrPtr -> IO (Ptr LRocksDB)

foreign import ccall safe "rocksdb/c.h rocksdb_open_column_families"
  c_rocksdb_open_column_families :: Ptr LOptions
                                 -> DBName
                                 -> CInt
                                 -> Ptr CFName
                                 -> Ptr (Ptr LOptions)
                                 -> Ptr (Ptr LColumnFamily)
                                 -> ErrPtr
                                 -> IO (Ptr LRocksDB)

foreign import ccall safe "rocksdb/c.h &rocksdb_close"
  c_rocksdb_close :: FunPtr (Ptr LRocksDB -> IO ())

foreign import ccall safe "rocksdb/c.h rocksdb_create_column_family"
  c_rocksdb_create_column_family :: Ptr LRocksDB
                                 -> Ptr LOptions
                                 -> CFName
                                 -> ErrPtr
                                 -> IO (Ptr LColumnFamily)

foreign import ccall safe "rocksdb/c.h rocksdb_drop_column_family"
  c_rocksdb_drop_column_family :: Ptr LRocksDB
                               -> Ptr LColumnFamily
                               -> ErrPtr
                               -> IO ()

foreign import ccall safe "rocksdb/c.h rocksdb_column_family_handle_destroy"
  c_rocksdb_column_family_handle_destroy :: Ptr LColumnFamily -> IO ()

foreign import ccall safe "rocksdb/c.h rocksdb_put"
  c_rocksdb_put :: Ptr LRocksDB
                -> Ptr LWriteOpts
                -> Key -> CSize
                -> Val -> CSize
                -> ErrPtr
                -> IO ()

foreign import ccall safe "rocksdb/c.h rocksdb_put_cf"
  c_rocksdb_put_cf :: Ptr LRocksDB
                   -> Ptr LWriteOpts
                   -> Ptr LColumnFamily
                   -> Key -> CSize
                   -> Val -> CSize
                   -> ErrPtr
                   -> IO ()

foreign import ccall safe "rocksdb/c.h rocksdb_delete"
  c_rocksdb_delete :: Ptr LRocksDB
                   -> Ptr LWriteOpts
                   -> Key -> CSize
                   -> ErrPtr
                   -> IO ()

foreign import ccall safe "rocksdb/c.h rocksdb_delete_cf"
  c_rocksdb_delete_cf :: Ptr LRocksDB
                      -> Ptr LWriteOpts
                      -> Ptr LColumnFamily
                      -> Key -> CSize
                      -> ErrPtr
                      -> IO ()

foreign import ccall safe "rocksdb/c.h rocksdb_write"
  c_rocksdb_write :: Ptr LRocksDB
                  -> Ptr LWriteOpts
                  -> Ptr LWriteBatch
                  -> ErrPtr
                  -> IO ()

-- | Returns NULL if not found. A malloc()ed array otherwise.
-- Stores the length of the array in *vallen.
foreign import ccall safe "rocksdb/c.h rocksdb_get"
  c_rocksdb_get :: Ptr LRocksDB
                -> Ptr LReadOpts
                -> Key -> CSize
                -> Ptr CSize        -- ^ value length
                -> ErrPtr
                -> IO CString

foreign import ccall safe "rocksdb/c.h rocksdb_get_cf"
  c_rocksdb_get_cf :: Ptr LRocksDB
                   -> Ptr LReadOpts
                   -> Ptr LColumnFamily
                   -> Key -> CSize
                   -> Ptr CSize        -- ^ value length
                   -> ErrPtr
                   -> IO CString

foreign import ccall safe "rocksdb/c.h rocksdb_create_snapshot"
  c_rocksdb_create_snapshot :: Ptr LRocksDB -> IO (Ptr LSnapshot)

foreign import ccall safe "rocksdb/c.h rocksdb_release_snapshot"
  c_rocksdb_release_snapshot :: Ptr LRocksDB -> Ptr LSnapshot -> IO ()

-- | Returns NULL if property name is unknown. Else returns a pointer to a
-- malloc()-ed null-terminated value.
foreign import ccall safe "rocksdb/c.h rocksdb_property_value"
  c_rocksdb_property_value :: Ptr LRocksDB -> CString -> IO CString

foreign import ccall safe "rocksdb/c.h rocksdb_approximate_sizes"
  c_rocksdb_approximate_sizes :: Ptr LRocksDB
                              -> CInt
                                 -- ^ num ranges
                              -> Ptr CString -> Ptr CSize
                                 -- ^ range start keys (array)
                              -> Ptr CString -> Ptr CSize
                                 -- ^ range limit keys (array)
                              -> Ptr Word64
                                 -- ^ array of approx. sizes of ranges
                              -> IO ()

foreign import ccall safe "rocksdb/c.h rocksdb_destroy_db"
  c_rocksdb_destroy_db :: Ptr LOptions -> DBName -> ErrPtr -> IO ()

foreign import ccall safe "rocksdb/c.h rocksdb_repair_db"
  c_rocksdb_repair_db :: Ptr LOptions -> DBName -> ErrPtr -> IO ()


--
-- Iterator
--

foreign import ccall safe "rocksdb/c.h rocksdb_create_iterator"
  c_rocksdb_create_iterator :: Ptr LRocksDB
                            -> Ptr LReadOpts
                            -> IO (Ptr LIterator)

foreign import ccall safe "rocksdb/c.h &rocksdb_iter_destroy"
  c_rocksdb_iter_destroy :: FunPtr (Ptr LIterator -> IO ())

foreign import ccall safe "rocksdb/c.h rocksdb_iter_valid"
  c_rocksdb_iter_valid :: Ptr LIterator -> IO CUChar

foreign import ccall safe "rocksdb/c.h rocksdb_iter_seek_to_first"
  c_rocksdb_iter_seek_to_first :: Ptr LIterator -> IO ()

foreign import ccall safe "rocksdb/c.h rocksdb_iter_seek_to_last"
  c_rocksdb_iter_seek_to_last :: Ptr LIterator -> IO ()

foreign import ccall safe "rocksdb/c.h rocksdb_iter_seek"
  c_rocksdb_iter_seek :: Ptr LIterator -> Key -> CSize -> IO ()

foreign import ccall safe "rocksdb/c.h rocksdb_iter_next"
  c_rocksdb_iter_next :: Ptr LIterator -> IO ()

foreign import ccall safe "rocksdb/c.h rocksdb_iter_prev"
  c_rocksdb_iter_prev :: Ptr LIterator -> IO ()

foreign import ccall safe "rocksdb/c.h rocksdb_iter_key"
  c_rocksdb_iter_key :: Ptr LIterator -> Ptr CSize -> IO Key

foreign import ccall safe "rocksdb/c.h rocksdb_iter_value"
  c_rocksdb_iter_value :: Ptr LIterator -> Ptr CSize -> IO Val

foreign import ccall safe "rocksdb/c.h rocksdb_iter_get_error"
  c_rocksdb_iter_get_error :: Ptr LIterator -> ErrPtr -> IO ()


--
-- Write batch
--

foreign import ccall safe "rocksdb/c.h rocksdb_writebatch_create"
  c_rocksdb_writebatch_create :: IO (Ptr LWriteBatch)

foreign import ccall safe "rocksdb/c.h rocksdb_writebatch_destroy"
  c_rocksdb_writebatch_destroy :: Ptr LWriteBatch -> IO ()

foreign import ccall safe "rocksdb/c.h rocksdb_writebatch_clear"
  c_rocksdb_writebatch_clear :: Ptr LWriteBatch -> IO ()

foreign import ccall safe "rocksdb/c.h rocksdb_writebatch_put"
  c_rocksdb_writebatch_put :: Ptr LWriteBatch
                           -> Key -> CSize
                           -> Val -> CSize
                           -> IO ()

foreign import ccall safe "rocksdb/c.h rocksdb_writebatch_put_cf"
  c_rocksdb_writebatch_put_cf :: Ptr LWriteBatch
                              -> Ptr LColumnFamily
                              -> Key -> CSize
                              -> Val -> CSize
                              -> IO ()

foreign import ccall safe "rocksdb/c.h rocksdb_writebatch_delete"
  c_rocksdb_writebatch_delete :: Ptr LWriteBatch
                              -> Key -> CSize -> IO ()

foreign import ccall safe "rocksdb/c.h rocksdb_writebatch_delete_cf"
  c_rocksdb_writebatch_delete_cf :: Ptr LWriteBatch
                                 -> Ptr LColumnFamily
                                 -> Key
                                 -> CSize
                                 -> IO ()

--
-- Options
--

foreign import ccall safe "rocksdb/c.h rocksdb_options_create"
  c_rocksdb_options_create :: IO (Ptr LOptions)

foreign import ccall "rocksdb/c.h &rocksdb_options_destroy"
  c_rocksdb_options_destroy :: FunPtr (Ptr LOptions -> IO ())

foreign import ccall safe "rocksdb/c.h rocksdb_options_set_create_if_missing"
  c_rocksdb_options_set_create_if_missing :: Ptr LOptions -> CBool -> IO ()

foreign import ccall safe "rocksdb/c.h rocksdb_options_set_error_if_exists"
  c_rocksdb_options_set_error_if_exists :: Ptr LOptions -> CBool -> IO ()

foreign import ccall safe "rocksdb/c.h rocksdb_options_set_paranoid_checks"
  c_rocksdb_options_set_paranoid_checks :: Ptr LOptions -> CBool -> IO ()

foreign import ccall safe "rocksdb/c.h rocksdb_options_set_max_open_files"
  c_rocksdb_options_set_max_open_files :: Ptr LOptions -> CInt -> IO ()

foreign import ccall safe "rocksdb/c.h rocksdb_options_set_prefix_extractor"
  c_rocksdb_options_set_prefix_extractor :: Ptr LOptions
                                         -> Ptr LPrefixExtract
                                         -> IO ()

--
-- Prefix Extractor
--

foreign import ccall safe "rocksdb/c.h rocksdb_slicetransform_create_fixed_prefix"
  c_rocksdb_slicetransform_create_fixed_prefix :: CSize -> IO (Ptr LPrefixExtract)

foreign import ccall safe "rocksdb/c.h &rocksdb_slicetransform_destroy"
  c_rocksdb_slicetransform_destroy :: FunPtr (Ptr LPrefixExtract -> IO ())

--
-- Bloom Filter
--

foreign import ccall safe "rocksdb/c.h &rocksdb_filterpolicy_destroy"
  c_rocksdb_filterpolicy_destroy :: FunPtr (Ptr LBloomFilter -> IO ())

foreign import ccall safe "rocksdb/c.h rocksdb_filterpolicy_create_bloom"
  c_rocksdb_filterpolicy_create_bloom :: CInt -> IO (Ptr LBloomFilter)

--
-- Read options
--

foreign import ccall safe "rocksdb/c.h rocksdb_readoptions_create"
  c_rocksdb_readoptions_create :: IO (Ptr LReadOpts)

foreign import ccall safe "rocksdb/c.h &rocksdb_readoptions_destroy"
  c_rocksdb_readoptions_destroy :: FunPtr (Ptr LReadOpts -> IO ())

foreign import ccall safe "rocksdb/c.h rocksdb_readoptions_set_snapshot"
  c_rocksdb_readoptions_set_snapshot :: Ptr LReadOpts -> Ptr LSnapshot -> IO ()


--
-- Write options
--

foreign import ccall safe "rocksdb/c.h rocksdb_writeoptions_create"
  c_rocksdb_writeoptions_create :: IO (Ptr LWriteOpts)

foreign import ccall safe "rocksdb/c.h &rocksdb_writeoptions_destroy"
  c_rocksdb_writeoptions_destroy :: FunPtr (Ptr LWriteOpts -> IO ())

--
-- Free
--

foreign import ccall safe "rocksdb/c.h rocksdb_free"
  c_rocksdb_free :: CString -> IO ()
