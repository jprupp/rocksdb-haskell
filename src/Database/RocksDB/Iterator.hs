{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TupleSections #-}

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
module Database.RocksDB.Iterator
  ( Iterator,
    -- * Bracket-style iteration
    withIter,
    withIterCF,
    withIterSnap,
    withIterSnapCF,
    -- * ResourceT-style iteration
    iter,
    iterCF,
    iterator,
    iteratorSnap,
    -- * Manual iterator management
    createIterator,
    createIteratorSnap,
    destroyIterator,
    destroyReadOpts,
    -- * Iterator operations
    iterEntry,
    iterFirst,
    iterGetError,
    iterKey,
    iterLast,
    iterNext,
    iterPrev,
    iterSeek,
    iterValid,
    iterValue,
  )
where

import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Unsafe qualified as BU
import Database.RocksDB.C
import Database.RocksDB.Internal
import Foreign
import Foreign.C.Error (throwErrnoIfNull)
import Foreign.C.String (CString)
import Foreign.C.Types (CSize)
import UnliftIO
import UnliftIO.Resource

-- | Create 'Iterator' and use it.
--
-- Note that an 'Iterator' creates a snapshot of the database implicitly, so
-- updates written after the iterator was created are not visible. You may,
-- however, specify an older 'Snapshot' in the 'ReadOptions'.
--
-- Iterator should not be used after computation ends.
withIter :: (MonadUnliftIO m) => DB -> (Iterator -> m a) -> m a
withIter db = withIterCommon db Nothing Nothing

withIterCF :: (MonadUnliftIO m) => DB -> ColumnFamily -> (Iterator -> m a) -> m a
withIterCF db cf = withIterCommon db Nothing (Just cf)

-- | Create 'Iterator' on a specific snapshot.
--
-- If 'Nothing' is passed, the iterator creates its own implicit snapshot.
-- If 'Just snapshot' is passed, the iterator uses the provided snapshot,
-- enabling consistent reads across multiple iterators and point queries.
--
-- Iterator should not be used after computation ends.
withIterSnap
    :: (MonadUnliftIO m)
    => DB
    -> Maybe Snapshot
    -> (Iterator -> m a)
    -> m a
withIterSnap db msnap = withIterCommon db msnap Nothing

-- | Create 'Iterator' on a column family with a specific snapshot.
--
-- If 'Nothing' is passed for snapshot, the iterator creates its own implicit snapshot.
-- If 'Just snapshot' is passed, the iterator uses the provided snapshot.
withIterSnapCF
    :: (MonadUnliftIO m)
    => DB
    -> Maybe Snapshot
    -> ColumnFamily
    -> (Iterator -> m a)
    -> m a
withIterSnapCF db msnap cf = withIterCommon db msnap (Just cf)

-- | Variation on 'iterator' below.
iter :: (MonadIO m, MonadResource m) => DB -> m Iterator
iter db = iterator db Nothing

iterCF :: (MonadIO m, MonadResource m) => DB -> ColumnFamily -> m Iterator
iterCF db cf = iterator db (Just cf)

withIterCommon
    :: (MonadUnliftIO m)
    => DB
    -> Maybe Snapshot
    -> Maybe ColumnFamily
    -> (Iterator -> m a)
    -> m a
withIterCommon DB{rocksDB = rocks_db, readOpts = read_opts} msnap mcf f =
    case msnap of
        Nothing ->
            -- Use DB's readOpts (iterator creates implicit snapshot)
            bracket (create_iterator read_opts) destroy_iterator f
        Just snap ->
            -- Create temporary ReadOpts with the provided snapshot
            bracket create_read_opts destroy_read_opts $ \ro ->
                bracket (create_iterator ro) destroy_iterator f
          where
            create_read_opts = liftIO $ do
                ro <- c_rocksdb_readoptions_create
                c_rocksdb_readoptions_set_snapshot ro snap
                return ro
            destroy_read_opts = liftIO . c_rocksdb_readoptions_destroy
  where
    destroy_iterator = liftIO . c_rocksdb_iter_destroy
    create_iterator ro = liftIO $
        throwErrnoIfNull "create_iterator" $ case mcf of
            Just cf -> c_rocksdb_create_iterator_cf rocks_db ro cf
            Nothing -> c_rocksdb_create_iterator rocks_db ro

-- | Iterator is not valid outside of 'ResourceT' context.
iterator
    :: (MonadIO m, MonadResource m)
    => DB
    -> Maybe ColumnFamily
    -> m Iterator
iterator db mcf =
    snd <$> allocate (createIterator db mcf) destroyIterator

-- | Create iterator on a specific snapshot within 'ResourceT'.
--
-- If 'Nothing' is passed, the iterator creates its own implicit snapshot.
-- If 'Just snapshot' is passed, the iterator uses the provided snapshot.
iteratorSnap
    :: (MonadIO m, MonadResource m)
    => DB
    -> Maybe Snapshot
    -> Maybe ColumnFamily
    -> m Iterator
iteratorSnap db msnap mcf = case msnap of
    Nothing -> iterator db mcf
    Just snap -> do
        -- Allocate ReadOpts as a resource
        (_, ro) <- allocate
            (liftIO $ do
                ro <- c_rocksdb_readoptions_create
                c_rocksdb_readoptions_set_snapshot ro snap
                return ro)
            (liftIO . c_rocksdb_readoptions_destroy)
        -- Allocate iterator using the snapshot-aware ReadOpts
        snd <$> allocate
            (createIteratorWithOpts db ro mcf)
            destroyIterator

-- | Manually create unmanaged iterator.
createIterator :: (MonadIO m) => DB -> Maybe ColumnFamily -> m Iterator
createIterator DB{rocksDB = rocks_db, readOpts = read_opts} mcf = liftIO $
    throwErrnoIfNull "create_iterator" $ case mcf of
        Just cf -> c_rocksdb_create_iterator_cf rocks_db read_opts cf
        Nothing -> c_rocksdb_create_iterator rocks_db read_opts

-- | Manually create unmanaged iterator on a specific snapshot.
--
-- If 'Nothing' is passed, behaves like 'createIterator'.
-- If 'Just snapshot' is passed, creates a 'ReadOpts' tied to that snapshot.
--
-- Returns both the 'Iterator' and the 'ReadOpts' that was created.
-- Both must be destroyed: use 'destroyIterator' for the iterator,
-- and 'destroyReadOpts' for the read options.
--
-- For automatic resource management, prefer 'withIterSnap' or 'iteratorSnap'.
createIteratorSnap
    :: (MonadIO m)
    => DB
    -> Maybe Snapshot
    -> Maybe ColumnFamily
    -> m (Iterator, Maybe ReadOpts)
createIteratorSnap db msnap mcf = case msnap of
    Nothing -> (, Nothing) <$> createIterator db mcf
    Just snap -> liftIO $ do
        ro <- c_rocksdb_readoptions_create
        c_rocksdb_readoptions_set_snapshot ro snap
        it <- createIteratorWithOpts db ro mcf
        return (it, Just ro)

-- | Internal: create iterator with explicit ReadOpts.
createIteratorWithOpts
    :: (MonadIO m)
    => DB
    -> ReadOpts
    -> Maybe ColumnFamily
    -> m Iterator
createIteratorWithOpts DB{rocksDB = rocks_db} ro mcf = liftIO $
    throwErrnoIfNull "create_iterator" $ case mcf of
        Just cf -> c_rocksdb_create_iterator_cf rocks_db ro cf
        Nothing -> c_rocksdb_create_iterator rocks_db ro

-- | Destroy unmanaged iterator.
destroyIterator :: (MonadIO m) => Iterator -> m ()
destroyIterator = liftIO . c_rocksdb_iter_destroy

-- | An iterator is either positioned at a key/value pair, or not valid. This
-- function returns /true/ iff the iterator is valid.
iterValid :: (MonadIO m) => Iterator -> m Bool
iterValid iter_ptr = liftIO $ do
  x <- c_rocksdb_iter_valid iter_ptr
  return (x /= 0)

-- | Position at the first key in the source that is at or past target. The
-- iterator is /valid/ after this call iff the source contains an entry that
-- comes at or past target.
iterSeek :: (MonadIO m) => Iterator -> ByteString -> m ()
iterSeek iter_ptr key = liftIO $
  BU.unsafeUseAsCStringLen key $ \(key_ptr, klen) ->
    c_rocksdb_iter_seek iter_ptr key_ptr (intToCSize klen)

-- | Position at the first key in the source. The iterator is /valid/ after this
-- call iff the source is not empty.
iterFirst :: (MonadIO m) => Iterator -> m ()
iterFirst = liftIO . c_rocksdb_iter_seek_to_first

-- | Position at the last key in the source. The iterator is /valid/ after this
-- call iff the source is not empty.
iterLast :: (MonadIO m) => Iterator -> m ()
iterLast = liftIO . c_rocksdb_iter_seek_to_last

-- | Moves to the next entry in the source. After this call, 'iterValid' is
-- /true/ iff the iterator was not positioned at the last entry in the source.
--
-- If the iterator is not valid, this function does nothing. Note that this is a
-- shortcoming of the C API: an 'iterPrev' might still be possible, but we can't
-- determine if we're at the last or first entry.
iterNext :: (MonadIO m) => Iterator -> m ()
iterNext iter_ptr = liftIO $ c_rocksdb_iter_next iter_ptr

-- | Moves to the previous entry in the source. After this call, 'iterValid' is
-- /true/ iff the iterator was not positioned at the first entry in the source.
--
-- If the iterator is not valid, this function does nothing. Note that this is a
-- shortcoming of the C API: an 'iterNext' might still be possible, but we can't
-- determine if we're at the last or first entry.
iterPrev :: (MonadIO m) => Iterator -> m ()
iterPrev iter_ptr = liftIO $ c_rocksdb_iter_prev iter_ptr

-- | Return the key for the current entry if the iterator is currently
-- positioned at an entry, ie. 'iterValid'.
iterKey :: (MonadIO m) => Iterator -> m (Maybe ByteString)
iterKey it = liftIO $ iterString it c_rocksdb_iter_key

-- | Return the value for the current entry if the iterator is currently
-- positioned at an entry, ie. 'iterValid'.
iterValue :: (MonadIO m) => Iterator -> m (Maybe ByteString)
iterValue it = liftIO $ iterString it c_rocksdb_iter_value

-- | Return the current entry as a pair, if the iterator is currently positioned
-- at an entry, ie. 'iterValid'.
iterEntry :: (MonadIO m) => Iterator -> m (Maybe (ByteString, ByteString))
iterEntry it = liftIO $ do
  mkey <- iterKey it
  mval <- iterValue it
  return $ (,) <$> mkey <*> mval

-- | Check for errors
--
-- Note that this captures somewhat severe errors such as a corrupted database.
iterGetError :: (MonadIO m) => Iterator -> m (Maybe ByteString)
iterGetError iter_ptr = liftIO $ alloca $ \err_ptr -> do
  poke err_ptr nullPtr
  c_rocksdb_iter_get_error iter_ptr err_ptr
  err_str <- peek err_ptr
  if err_str == nullPtr
    then return Nothing
    else Just <$> BU.unsafePackMallocCString err_str

--
-- Internal
--

iterString ::
  Iterator ->
  (Iterator -> Ptr CSize -> IO CString) ->
  IO (Maybe ByteString)
iterString it f = do
  valid <- iterValid it
  if valid
    then alloca $ \len_ptr -> do
      str_ptr <- f it len_ptr
      if str_ptr == nullPtr
        then return Nothing
        else do
          len <- peek len_ptr
          Just <$> B.packCStringLen (str_ptr, cSizeToInt len)
    else return Nothing