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

import           Control.Monad
import           Data.Default
import           Database.RocksDB.C
import           UnliftIO
import           UnliftIO.Foreign

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
                     } deriving (Eq, Show)

instance Default Config where
    def = Config { createIfMissing  = False
                 , errorIfExists    = False
                 , paranoidChecks   = False
                 , maxFiles         = Nothing
                 , prefixLength     = Nothing
                 }

withOptions :: MonadUnliftIO m => Config -> (Options -> m a) -> m a
withOptions Config {..} f =
    bracket create_opts destroy_opts (f . fst)
  where
    destroy_opts (opts_ptr, maybe_pfx_extract) = liftIO $ do
        c_rocksdb_options_destroy opts_ptr
        forM_ maybe_pfx_extract c_rocksdb_slicetransform_destroy
    create_opts = liftIO $ do
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
        maybe_pfx_extract <- case prefixLength of
            Nothing -> return Nothing
            Just n -> do
                pfx_extract <- c_rocksdb_slicetransform_create_fixed_prefix
                               (intToCSize n)
                return $ Just pfx_extract
        return (opts_ptr, maybe_pfx_extract)

withOptionsCF :: MonadUnliftIO m => [Config] -> ([Options] -> m a) -> m a
withOptionsCF cfgs f =
    go [] cfgs
  where
    go acc [] = f (reverse acc)
    go acc (c:cs) = withOptions c $ \o -> go (o:acc) cs

withReadOpts :: MonadUnliftIO m => Maybe Snapshot -> (ReadOpts -> m a) -> m a
withReadOpts maybe_snap_ptr =
    bracket
    create_read_opts
    (liftIO . c_rocksdb_readoptions_destroy)
  where
    create_read_opts = liftIO $ do
        read_opts_ptr <- c_rocksdb_readoptions_create
        forM_ maybe_snap_ptr $ c_rocksdb_readoptions_set_snapshot read_opts_ptr
        return read_opts_ptr

withWriteOpts :: MonadUnliftIO m => (WriteOpts -> m a) -> m a
withWriteOpts =
    bracket
    (liftIO c_rocksdb_writeoptions_create)
    (liftIO . c_rocksdb_writeoptions_destroy)

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
