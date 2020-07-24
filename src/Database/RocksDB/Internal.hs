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

withOptions :: MonadUnliftIO m => Config -> (Options -> m a) -> m a
withOptions Config {..} f =
    with_opts $ \opts -> do
        liftIO $ do
            slice <- bloom
            block_opts opts slice
            pfx_extract opts
            max_files opts
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
        (liftIO c_rocksdb_options_create)
        (liftIO . c_rocksdb_options_destroy)
    block_opts _ Nothing = return ()
    block_opts opts (Just slice) = liftIO $ do
        block <- c_rocksdb_block_based_options_create
        c_rocksdb_block_based_options_set_filter_policy block slice
        c_rocksdb_options_set_block_based_table_factory opts block
    bloom =
        if bloomFilter
        then Just <$> c_rocksdb_filterpolicy_create_bloom_full 10
        else return Nothing
    pfx_extract opts =
        case prefixLength of
            Nothing -> return ()
            Just len -> liftIO $ do
                p <- c_rocksdb_slicetransform_create_fixed_prefix
                     (intToCSize len)
                c_rocksdb_options_set_prefix_extractor opts p
    max_files opts =
        case maxFiles of
            Nothing -> return ()
            Just i -> c_rocksdb_options_set_max_open_files opts (intToCInt i)


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
