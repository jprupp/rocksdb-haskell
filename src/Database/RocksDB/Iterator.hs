{-# LANGUAGE LambdaCase #-}
-- |
-- Module      : Database.RocksDB.Iterator
-- Copyright   : (c) 2012-2013 The leveldb-haskell Authors
--               (c) 2014-2020 The rocksdb-haskell Authors
-- License     : BSD3
-- Maintainer  : jprupp@protonmail.ch
-- Stability   : experimental
-- Portability : non-portable
--
-- Iterating over key ranges.
--

module Database.RocksDB.Iterator
    ( Iterator
    , withIter
    , iterEntry
    , iterFirst
    , iterGetError
    , iterKey
    , iterLast
    , iterNext
    , iterPrev
    , iterSeek
    , iterValid
    , iterValue
    ) where

import           Control.Monad             (when)
import           Data.ByteString           (ByteString)
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Char8     as BC
import           Database.RocksDB.C
import           Database.RocksDB.Internal
import           Foreign
import           Foreign.C.Error           (throwErrnoIfNull)
import           Foreign.C.String          (CString, peekCString)
import           Foreign.C.Types           (CSize)
import           UnliftIO

-- | Create 'Iterator' and use it.
--
-- Note that an 'Iterator' creates a snapshot of the database implicitly, so
-- updates written after the iterator was created are not visible. You may,
-- however, specify an older 'Snapshot' in the 'ReadOptions'.
--
-- Iterator should not be used after computation ends.
withIter :: MonadUnliftIO m => DB -> (Iterator -> m a) -> m a
withIter DB{rocksDB = rocks_db, readOpts = read_opts} =
    bracket create_iterator destroy_iterator
  where
    destroy_iterator = liftIO . c_rocksdb_iter_destroy
    create_iterator = liftIO $
        throwErrnoIfNull "create_iterator" $
        c_rocksdb_create_iterator rocks_db read_opts

-- | An iterator is either positioned at a key/value pair, or not valid. This
-- function returns /true/ iff the iterator is valid.
iterValid :: MonadIO m => Iterator -> m Bool
iterValid iter_ptr = liftIO $ do
    x <- c_rocksdb_iter_valid iter_ptr
    return (x /= 0)

-- | Position at the first key in the source that is at or past target. The
-- iterator is /valid/ after this call iff the source contains an entry that
-- comes at or past target.
iterSeek :: MonadIO m => Iterator -> ByteString -> m ()
iterSeek iter_ptr key = liftIO $
    BS.useAsCStringLen key $ \(key_ptr, klen) ->
    c_rocksdb_iter_seek iter_ptr key_ptr (intToCSize klen)

-- | Position at the first key in the source. The iterator is /valid/ after this
-- call iff the source is not empty.
iterFirst :: MonadIO m => Iterator -> m ()
iterFirst = liftIO . c_rocksdb_iter_seek_to_first

-- | Position at the last key in the source. The iterator is /valid/ after this
-- call iff the source is not empty.
iterLast :: MonadIO m => Iterator -> m ()
iterLast = liftIO . c_rocksdb_iter_seek_to_last

-- | Moves to the next entry in the source. After this call, 'iterValid' is
-- /true/ iff the iterator was not positioned at the last entry in the source.
--
-- If the iterator is not valid, this function does nothing. Note that this is a
-- shortcoming of the C API: an 'iterPrev' might still be possible, but we can't
-- determine if we're at the last or first entry.
iterNext :: MonadIO m => Iterator -> m ()
iterNext iter_ptr = liftIO $ do
    valid <- c_rocksdb_iter_valid iter_ptr
    when (valid /= 0) $ c_rocksdb_iter_next iter_ptr

-- | Moves to the previous entry in the source. After this call, 'iterValid' is
-- /true/ iff the iterator was not positioned at the first entry in the source.
--
-- If the iterator is not valid, this function does nothing. Note that this is a
-- shortcoming of the C API: an 'iterNext' might still be possible, but we can't
-- determine if we're at the last or first entry.
iterPrev :: MonadIO m => Iterator -> m ()
iterPrev iter_ptr = liftIO $ do
    valid <- c_rocksdb_iter_valid iter_ptr
    when (valid /= 0) $ c_rocksdb_iter_prev iter_ptr

-- | Return the key for the current entry if the iterator is currently
-- positioned at an entry, ie. 'iterValid'.
iterKey :: MonadIO m => Iterator -> m (Maybe ByteString)
iterKey = liftIO . flip iterString c_rocksdb_iter_key

-- | Return the value for the current entry if the iterator is currently
-- positioned at an entry, ie. 'iterValid'.
iterValue :: MonadIO m => Iterator -> m (Maybe ByteString)
iterValue = liftIO . flip iterString c_rocksdb_iter_value

-- | Return the current entry as a pair, if the iterator is currently positioned
-- at an entry, ie. 'iterValid'.
iterEntry :: MonadIO m => Iterator -> m (Maybe (ByteString, ByteString))
iterEntry iter = liftIO $ do
    mkey <- iterKey iter
    mval <- iterValue iter
    return $ (,) <$> mkey <*> mval

-- | Check for errors
--
-- Note that this captures somewhat severe errors such as a corrupted database.
iterGetError :: MonadIO m => Iterator -> m (Maybe ByteString)
iterGetError iter_ptr = liftIO . alloca $ \err_ptr -> do
    poke err_ptr nullPtr
    c_rocksdb_iter_get_error iter_ptr err_ptr
    erra <- peek err_ptr
    if erra == nullPtr
        then return Nothing
        else do
            err <- peekCString erra
            freeCString erra
            return $ Just (BC.pack err)

--
-- Internal
--

iterString :: Iterator
           -> (Iterator -> Ptr CSize -> IO CString)
           -> IO (Maybe ByteString)
iterString iter_ptr f =
    c_rocksdb_iter_valid iter_ptr >>= \case
        0 -> return Nothing
        _ -> alloca $ \len_ptr ->
            f iter_ptr len_ptr >>= \ptr ->
            if ptr == nullPtr
                then return Nothing
                else do
                    len <- peek len_ptr
                    ret <- BS.packCStringLen (ptr, cSizeToInt len)
                    freeCString ptr
                    return $ Just ret
