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
    , withReadOpts
    , writeOpts

    -- * Utilities
    , freeCString
    , throwIfErr
    , cSizeToInt
    , intToCSize
    , intToCInt
    , cIntToInt
    , boolToNum
    ) where

import           Control.Monad      (when)
import           Data.Default
import           Database.RocksDB.C
import           System.IO.Unsafe
import           UnliftIO
import           UnliftIO.Foreign

type RocksDB       = ForeignPtr LRocksDB
type Options'      = ForeignPtr LOptions
type ColumnFamily  = ForeignPtr LColumnFamily
type PrefixExtract = ForeignPtr LPrefixExtract
type Snapshot      = ForeignPtr LSnapshot
type WriteOpts     = Ptr LWriteOpts

data ReadOpts = ReadOpts !(ForeignPtr LReadOpts) !Snapshot
              | DefReadOpts !(Ptr LReadOpts)
              deriving (Eq, Show)

data Config = Config { createIfMissing :: !Bool
                     , errorIfExists   :: !Bool
                     , paranoidChecks  :: !Bool
                     , maxFiles        :: !(Maybe Int)
                     , prefixLength    :: !(Maybe Int)
                     } deriving (Eq, Show)

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

withReadOpts :: MonadUnliftIO m => ReadOpts -> (Ptr LReadOpts -> m a) -> m a
withReadOpts (ReadOpts fptr _) = withForeignPtr fptr
withReadOpts (DefReadOpts ptr) = ($ ptr)

writeOpts :: WriteOpts
writeOpts = unsafePerformIO c_rocksdb_writeoptions_create
{-# NOINLINE writeOpts #-}

defReadOpts :: ReadOpts
defReadOpts = DefReadOpts $ unsafePerformIO c_rocksdb_readoptions_create
{-# NOINLINE defReadOpts #-}

instance Default ReadOpts where
    def = defReadOpts

newReadOpts :: MonadIO m => Maybe Snapshot -> m ReadOpts
newReadOpts Nothing = return defReadOpts
newReadOpts (Just snap_fptr) = liftIO $ do
    read_opts_ptr <- c_rocksdb_readoptions_create
    withForeignPtr snap_fptr $
        c_rocksdb_readoptions_set_snapshot read_opts_ptr
    ropts_fptr <- newForeignPtr c_rocksdb_readoptions_destroy read_opts_ptr
    return $ ReadOpts ropts_fptr snap_fptr

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
