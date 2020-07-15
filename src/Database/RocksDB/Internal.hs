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
    ( Options (..)
    , Config (..)
    , RocksDB
    , ColumnFamily
    , ReadOpts
    , WriteOpts

    -- * Smart constructors & extractors
    , newOptions
    , defaultOptions
    , newReadOpts
    , defaultReadOpts
    , newWriteOpts
    , defaultWriteOpts

    -- * Utilities
    , freeCString
    , throwIfErr
    , cSizeToInt
    , intToCSize
    , intToCInt
    , cIntToInt
    , boolToNum
    ) where

import           Control.Monad      (forM_, when)
import           Data.Default
import           Database.RocksDB.C
import           UnliftIO
import           UnliftIO.Foreign

type RocksDB       = ForeignPtr LRocksDB
type Options'      = ForeignPtr LOptions
type ColumnFamily  = ForeignPtr LColumnFamily
type ReadOpts      = ForeignPtr LReadOpts
type WriteOpts     = ForeignPtr LWriteOpts
type PrefixExtract = ForeignPtr LPrefixExtract

data Config = Config { createIfMissing :: !Bool
                     , errorIfExists   :: !Bool
                     , paranoidChecks  :: !Bool
                     , maxFiles        :: !(Maybe Int)
                     , prefixLength    :: !(Maybe Int)
                     }

instance Default Config where
    def = Config { createIfMissing  = False
                 , errorIfExists    = False
                 , paranoidChecks   = False
                 , maxFiles         = Nothing
                 , prefixLength     = Nothing
                 }

data Options = Options { config        :: !Config
                       , options       :: !Options'
                       , prefixExtract :: !(Maybe PrefixExtract)
                       }

defaultOptions :: MonadIO m => m Options
defaultOptions = newOptions def

newOptions :: MonadIO m => Config -> m Options
newOptions config@Config {..} = liftIO $ do
    opts_ptr <- c_rocksdb_options_create
    c_rocksdb_options_set_create_if_missing
        opts_ptr (boolToCBool createIfMissing)
    c_rocksdb_options_set_error_if_exists
        opts_ptr (boolToCBool errorIfExists)
    c_rocksdb_options_set_paranoid_checks
        opts_ptr (boolToCBool paranoidChecks)
    case maxFiles of
        Nothing -> return ()
        Just n  -> c_rocksdb_options_set_max_open_files
                   opts_ptr
                   (intToCInt n)
    prefixExtract <- case prefixLength of
        Nothing -> return Nothing
        Just n -> do
            pfx_extract_ptr <- c_rocksdb_slicetransform_create_fixed_prefix
                               (intToCSize n)
            Just <$>
                newForeignPtr
                c_rocksdb_slicetransform_destroy
                pfx_extract_ptr
    options <- newForeignPtr
               c_rocksdb_options_destroy
               opts_ptr
    return Options {..}

defaultWriteOpts :: MonadIO m => m WriteOpts
defaultWriteOpts = newWriteOpts False

newWriteOpts :: MonadIO m => Bool -> m WriteOpts
newWriteOpts sync = do
    write_opts_ptr <- liftIO c_rocksdb_writeoptions_create
    liftIO $ c_rocksdb_writeoptions_set_sync write_opts_ptr (boolToCBool sync)
    newForeignPtr c_rocksdb_writeoptions_destroy write_opts_ptr

defaultReadOpts :: MonadIO m => m ReadOpts
defaultReadOpts = newReadOpts False Nothing

newReadOpts :: MonadIO m => Bool -> Maybe Snapshot -> m ReadOpts
newReadOpts verify snapshot = liftIO $ do
    read_opts_ptr <- c_rocksdb_readoptions_create
    c_rocksdb_readoptions_set_verify_checksums read_opts_ptr (boolToCBool verify)
    forM_ snapshot $ c_rocksdb_readoptions_set_snapshot read_opts_ptr
    newForeignPtr c_rocksdb_readoptions_destroy read_opts_ptr

freeCString :: CString -> IO ()
freeCString = c_rocksdb_free

throwIfErr :: MonadUnliftIO m => String -> (ErrPtr -> m a) -> m a
throwIfErr s f = alloca $ \err_ptr -> do
    liftIO $ poke err_ptr nullPtr
    res  <- f err_ptr
    erra <- liftIO $ peek err_ptr
    when (erra /= nullPtr) $ do
        err <- liftIO $ peekCString erra
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
