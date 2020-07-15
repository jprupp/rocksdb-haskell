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
    , createIter
    , iterEntry
    , iterFirst
    , iterGetError
    , iterItems
    , iterKey
    , iterKeys
    , iterLast
    , iterNext
    , iterPrev
    , iterSeek
    , iterValid
    , iterValue
    , iterValues
    , mapIter
    ) where

import           Control.Monad             (when)
import           Data.ByteString           (ByteString)
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Char8     as BC
import qualified Data.ByteString.Unsafe    as BU
import           Data.Maybe                (catMaybes)
import           Database.RocksDB.C
import           Database.RocksDB.Internal
import           Foreign
import           Foreign.C.Error           (throwErrnoIfNull)
import           Foreign.C.String          (CString, peekCString)
import           Foreign.C.Types           (CSize)
import           UnliftIO

type Iterator' = ForeignPtr LIterator

data Iterator = Iterator !Iterator' !ReadOpts

-- | Create an 'Iterator'.
--
-- Note that an 'Iterator' creates a snapshot of the database implicitly, so
-- updates written after the iterator was created are not visible. You may,
-- however, specify an older 'Snapshot' in the 'ReadOptions'.
createIter :: MonadIO m => RocksDB -> ReadOpts -> m Iterator
createIter db ropts =
    liftIO $
    withForeignPtr db $ \db_ptr ->
    withForeignPtr ropts $ \ropts_ptr -> do
        iter_ptr <- throwErrnoIfNull "create_iterator" $
                    c_rocksdb_create_iterator db_ptr ropts_ptr
        iter_fptr <- newForeignPtr c_rocksdb_iter_destroy iter_ptr
        return $ Iterator iter_fptr ropts

-- | An iterator is either positioned at a key/value pair, or not valid. This
-- function returns /true/ iff the iterator is valid.
iterValid :: MonadIO m => Iterator -> m Bool
iterValid (Iterator iter_fptr _) =
    liftIO $ withForeignPtr iter_fptr $ \iter_ptr -> do
        x <- c_rocksdb_iter_valid iter_ptr
        return (x /= 0)

-- | Position at the first key in the source that is at or past target. The
-- iterator is /valid/ after this call iff the source contains an entry that
-- comes at or past target.
iterSeek :: MonadIO m => Iterator -> ByteString -> m ()
iterSeek (Iterator iter_fptr _) key =
    liftIO $ withForeignPtr iter_fptr $ \iter_ptr ->
    BU.unsafeUseAsCStringLen key $ \(key_ptr, klen) ->
    c_rocksdb_iter_seek iter_ptr key_ptr (intToCSize klen)

-- | Position at the first key in the source. The iterator is /valid/ after this
-- call iff the source is not empty.
iterFirst :: MonadIO m => Iterator -> m ()
iterFirst (Iterator iter_fptr _) =
    liftIO $ withForeignPtr iter_fptr $ \iter_ptr ->
    c_rocksdb_iter_seek_to_first iter_ptr

-- | Position at the last key in the source. The iterator is /valid/ after this
-- call iff the source is not empty.
iterLast :: MonadIO m => Iterator -> m ()
iterLast (Iterator iter_fptr _) =
    liftIO $ withForeignPtr iter_fptr $ \iter_ptr ->
    c_rocksdb_iter_seek_to_last iter_ptr

-- | Moves to the next entry in the source. After this call, 'iterValid' is
-- /true/ iff the iterator was not positioned at the last entry in the source.
--
-- If the iterator is not valid, this function does nothing. Note that this is a
-- shortcoming of the C API: an 'iterPrev' might still be possible, but we can't
-- determine if we're at the last or first entry.
iterNext :: MonadIO m => Iterator -> m ()
iterNext (Iterator iter_fptr _) =
    liftIO $ withForeignPtr iter_fptr $ \iter_ptr -> do
        valid <- c_rocksdb_iter_valid iter_ptr
        when (valid /= 0) $ c_rocksdb_iter_next iter_ptr

-- | Moves to the previous entry in the source. After this call, 'iterValid' is
-- /true/ iff the iterator was not positioned at the first entry in the source.
--
-- If the iterator is not valid, this function does nothing. Note that this is a
-- shortcoming of the C API: an 'iterNext' might still be possible, but we can't
-- determine if we're at the last or first entry.
iterPrev :: MonadIO m => Iterator -> m ()
iterPrev (Iterator iter_fptr _) =
    liftIO $ withForeignPtr iter_fptr $ \iter_ptr -> do
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
iterGetError (Iterator iter_fptr _) =
    liftIO $ withForeignPtr iter_fptr $ \iter_ptr ->
    alloca $ \err_ptr -> do
        poke err_ptr nullPtr
        c_rocksdb_iter_get_error iter_ptr err_ptr
        erra <- peek err_ptr
        if erra == nullPtr
            then return Nothing
            else do
                err <- peekCString erra
                return . Just . BC.pack $ err

-- | Map a function over an iterator, advancing the iterator forward and
-- returning the value. The iterator should be put in the right position prior
-- to calling the function.
--
-- Note that this function accumulates the result strictly, ie. it reads all
-- values into memory until the iterator is exhausted. This is most likely not
-- what you want for large ranges. You may consider using conduits instead.
mapIter :: MonadUnliftIO m => (Iterator -> m a) -> Iterator -> m [a]
mapIter f iter@(Iterator iter_fptr _) =
    withRunInIO $ \r -> withForeignPtr iter_fptr $ \iter_ptr ->
    r $ go iter_ptr []
  where
    go iter_ptr acc = do
        valid <- liftIO $ c_rocksdb_iter_valid iter_ptr
        if valid == 0
            then return acc
            else do
                val <- f iter
                ()  <- liftIO $ c_rocksdb_iter_next iter_ptr
                go iter_ptr (val : acc)

-- | Return a list of key and value tuples from an iterator. The iterator
-- should be put in the right position prior to calling this with the iterator.
--
-- See strictness remarks on 'mapIter'.
iterItems :: MonadUnliftIO m => Iterator -> m [(ByteString, ByteString)]
iterItems iter = catMaybes <$> mapIter iterEntry iter

-- | Return a list of key from an iterator. The iterator should be put
-- in the right position prior to calling this with the iterator.
--
-- See strictness remarks on 'mapIter'
iterKeys :: MonadUnliftIO m => Iterator -> m [ByteString]
iterKeys iter = catMaybes <$> mapIter iterKey iter

-- | Return a list of values from an iterator. The iterator should be put
-- in the right position prior to calling this with the iterator.
--
-- See strictness remarks on 'mapIter'
iterValues :: MonadUnliftIO m => Iterator -> m [ByteString]
iterValues iter = catMaybes <$> mapIter iterValue iter

--
-- Internal
--

iterString :: Iterator
           -> (Ptr LIterator -> Ptr CSize -> IO CString)
           -> IO (Maybe ByteString)
iterString (Iterator iter_fptr _) f =
    withForeignPtr iter_fptr $ \iter_ptr -> do
    valid <- c_rocksdb_iter_valid iter_ptr
    if valid == 0
        then return Nothing
        else alloca $ \len_ptr -> do
                 ptr <- f iter_ptr len_ptr
                 if ptr == nullPtr
                     then return Nothing
                     else do
                         len <- peek len_ptr
                         Just <$> BS.packCStringLen (ptr, cSizeToInt len)
