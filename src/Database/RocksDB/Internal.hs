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

module Database.RocksDB.Internal
    ( Config (..)
    , DB (..)

    -- * Smart constructors & extractors
    , withOptions
    , withOptionsCF
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
import           Database.RocksDB.C
import           Foreign
import           Foreign.C.String
import           Foreign.C.Types

data DB = DB { rocksDB        :: !RocksDB
             , columnFamilies :: ![ColumnFamily]
             , readOpts       :: !ReadOpts
             , writeOpts      :: !WriteOpts
             }

data Config = Config { createIfMissing :: !Bool
                     , errorIfExists   :: !Bool
                     , paranoidChecks  :: !Bool
                     , maxFiles        :: !(Maybe Int)
                     , prefixLength    :: !(Maybe Int)
                     , bloomFilter     :: !Bool
                     } deriving (Eq, Show)

instance Default Config where
    def = Config { createIfMissing  = False
                 , errorIfExists    = False
                 , paranoidChecks   = False
                 , maxFiles         = Nothing
                 , prefixLength     = Nothing
                 , bloomFilter      = False
                 }

withOptions :: Config -> (Options -> IO a) -> IO a
withOptions Config {..} f = with_opts $ \opts -> do
    when bloomFilter $ do
        fp <- c_rocksdb_filterpolicy_create_bloom_full 10
        bo <- c_rocksdb_block_based_options_create
        c_rocksdb_block_based_options_set_filter_policy bo fp
        c_rocksdb_options_set_block_based_table_factory opts bo
    forM_ prefixLength $ \l -> do
        t <- c_rocksdb_slicetransform_create_fixed_prefix (intToCSize l)
        c_rocksdb_options_set_prefix_extractor opts t
    forM_ maxFiles $
        c_rocksdb_options_set_max_open_files opts . intToCInt
    c_rocksdb_options_set_create_if_missing
        opts (boolToCBool createIfMissing)
    c_rocksdb_options_set_error_if_exists
        opts (boolToCBool errorIfExists)
    c_rocksdb_options_set_paranoid_checks
        opts (boolToCBool paranoidChecks)
    f opts
  where
    with_opts =
        bracket
        c_rocksdb_options_create
        c_rocksdb_options_destroy


withOptionsCF :: [Config] -> ([Options] -> IO a) -> IO a
withOptionsCF cfgs f =
    go [] cfgs
  where
    go acc [] = f (reverse acc)
    go acc (c:cs) = withOptions c $ \o -> go (o:acc) cs

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

withWriteOpts :: (WriteOpts -> IO a) -> IO a
withWriteOpts =
    bracket
    c_rocksdb_writeoptions_create
    c_rocksdb_writeoptions_destroy

freeCString :: CString -> IO ()
freeCString = c_rocksdb_free

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

boolToCBool :: Bool -> CBool
boolToCBool True  = 1
boolToCBool False = 0
{-# INLINE boolToCBool #-}

cSizeToInt :: CSize -> Int
cSizeToInt = fromIntegral
{-# INLINE cSizeToInt #-}

intToCSize :: Int -> CSize
intToCSize = fromIntegral
{-# INLINE intToCSize #-}

intToCInt :: Int -> CInt
intToCInt = fromIntegral
{-# INLINE intToCInt #-}

cIntToInt :: CInt -> Int
cIntToInt = fromIntegral
{-# INLINE cIntToInt #-}

boolToNum :: Num b => Bool -> b
boolToNum True  = fromIntegral (1 :: Int)
boolToNum False = fromIntegral (0 :: Int)
{-# INLINE boolToNum #-}
