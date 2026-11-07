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
--
-- An 'Iterator' walks the keys of a database or column family in ascending
-- byte order. It starts out not positioned anywhere; call 'iterFirst',
-- 'iterLast' or 'iterSeek' to position it, then 'iterNext' or 'iterPrev' to
-- move. Read the current entry with 'iterKey', 'iterValue' or 'iterEntry',
-- which return 'Nothing' once the iterator has moved past either end.
--
-- @
-- -- all entries whose key starts with "user:"
-- usersFrom :: DB -> IO [(ByteString, ByteString)]
-- usersFrom db = 'withIter' db $ \\it -> do
--     'iterSeek' it "user:"
--     let go acc = 'iterEntry' it >>= \\case
--             Just (k, v) | "user:" \`B.isPrefixOf\` k -> 'iterNext' it >> go ((k, v) : acc)
--             _ -> return (reverse acc)
--     go []
-- @
--
-- After iterating, check 'iterGetError' to tell the end of the data apart from
-- an error such as corruption.
module Database.RocksDB.Iterator
  ( Iterator,
    -- * Bracket-style iteration
    withIter,
    withIterCF,
    withIterSnap,
    withIterSnapCF,
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

import Control.Exception (bracket)
import Control.Monad (when)
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Unsafe qualified as BU
import Database.RocksDB.C
import Database.RocksDB.Internal
import Foreign
import Foreign.C.Error (throwErrnoIfNull)
import Foreign.C.String (CString)
import Foreign.C.Types (CSize)

-- | Create 'Iterator' and use it.
--
-- Note that an 'Iterator' creates a snapshot of the database implicitly, so
-- updates written after the iterator was created are not visible. You may,
-- however, specify an older 'Snapshot' in the 'ReadOptions'.
--
-- Iterator should not be used after computation ends.
withIter :: DB -> (Iterator -> IO a) -> IO a
withIter db = withIterCommon db Nothing Nothing

-- | Like 'withIter', but iterate over the given column family.
withIterCF :: DB -> ColumnFamily -> (Iterator -> IO a) -> IO a
withIterCF db cf = withIterCommon db Nothing (Just cf)

-- | Create 'Iterator' on a specific snapshot.
--
-- If 'Nothing' is passed, the iterator creates its own implicit snapshot.
-- If 'Just snapshot' is passed, the iterator uses the provided snapshot,
-- enabling consistent reads across multiple iterators and point queries.
--
-- Iterator should not be used after computation ends.
withIterSnap
    :: DB
    -> Maybe Snapshot
    -> (Iterator -> IO a)
    -> IO a
withIterSnap db msnap = withIterCommon db msnap Nothing

-- | Create 'Iterator' on a column family with a specific snapshot.
--
-- If 'Nothing' is passed for snapshot, the iterator creates its own implicit snapshot.
-- If 'Just snapshot' is passed, the iterator uses the provided snapshot.
withIterSnapCF
    :: DB
    -> Maybe Snapshot
    -> ColumnFamily
    -> (Iterator -> IO a)
    -> IO a
withIterSnapCF db msnap cf = withIterCommon db msnap (Just cf)

-- | Create an iterator over the default column family, or over a column family
-- if one is given, reading from a snapshot if one is given.
withIterCommon
    :: DB
    -> Maybe Snapshot
    -> Maybe ColumnFamily
    -> (Iterator -> IO a)
    -> IO a
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
            create_read_opts = do
                ro <- c_rocksdb_readoptions_create
                c_rocksdb_readoptions_set_snapshot ro snap
                return ro
            destroy_read_opts = c_rocksdb_readoptions_destroy
  where
    destroy_iterator = c_rocksdb_iter_destroy
    create_iterator ro =
        throwErrnoIfNull "create_iterator" $ case mcf of
            Just cf -> c_rocksdb_create_iterator_cf rocks_db ro cf
            Nothing -> c_rocksdb_create_iterator rocks_db ro

-- | Manually create unmanaged iterator, over the default column family or over
-- a column family if one is given. It uses the 'readOpts' of the 'DB', so if
-- the 'DB' comes from 'Database.RocksDB.withSnapshot' it reads from that
-- snapshot. It must be released with 'destroyIterator', and must not outlive
-- the 'DB'.
createIterator :: DB -> Maybe ColumnFamily -> IO Iterator
createIterator DB{rocksDB = rocks_db, readOpts = read_opts} mcf =
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
-- For automatic resource management, prefer 'withIterSnap'.
createIteratorSnap
    :: DB
    -> Maybe Snapshot
    -> Maybe ColumnFamily
    -> IO (Iterator, Maybe ReadOpts)
createIteratorSnap db msnap mcf = case msnap of
    Nothing -> (, Nothing) <$> createIterator db mcf
    Just snap -> do
        ro <- c_rocksdb_readoptions_create
        c_rocksdb_readoptions_set_snapshot ro snap
        it <- createIteratorWithOpts db ro mcf
        return (it, Just ro)

-- | Internal: create iterator with explicit ReadOpts.
createIteratorWithOpts
    :: DB
    -> ReadOpts
    -> Maybe ColumnFamily
    -> IO Iterator
createIteratorWithOpts DB{rocksDB = rocks_db} ro mcf =
    throwErrnoIfNull "create_iterator" $ case mcf of
        Just cf -> c_rocksdb_create_iterator_cf rocks_db ro cf
        Nothing -> c_rocksdb_create_iterator rocks_db ro

-- | Destroy unmanaged iterator created with 'createIterator' or
-- 'createIteratorSnap'. It must not be used afterwards.
destroyIterator :: Iterator -> IO ()
destroyIterator = c_rocksdb_iter_destroy

-- | An iterator is either positioned at a key/value pair, or not valid. This
-- function returns /true/ iff the iterator is valid.
iterValid :: Iterator -> IO Bool
iterValid iter_ptr = do
    x <- c_rocksdb_iter_valid iter_ptr
    return (x /= 0)

-- | Position at the first key in the source that is at or past target. The
-- iterator is /valid/ after this call iff the source contains an entry that
-- comes at or past target.
iterSeek :: Iterator -> ByteString -> IO ()
iterSeek iter_ptr key =
    BU.unsafeUseAsCStringLen key $ \(key_ptr, klen) ->
        c_rocksdb_iter_seek iter_ptr key_ptr (intToCSize klen)

-- | Position at the first key in the source. The iterator is /valid/ after this
-- call iff the source is not empty.
iterFirst :: Iterator -> IO ()
iterFirst = c_rocksdb_iter_seek_to_first

-- | Position at the last key in the source. The iterator is /valid/ after this
-- call iff the source is not empty.
iterLast :: Iterator -> IO ()
iterLast = c_rocksdb_iter_seek_to_last

-- | Moves to the next entry in the source. After this call, 'iterValid' is
-- /true/ iff the iterator was not positioned at the last entry in the source.
--
-- If the iterator is not valid, this function does nothing: RocksDB does not
-- allow moving an invalid iterator, so reposition it with 'iterSeek',
-- 'iterFirst' or 'iterLast' instead.
iterNext :: Iterator -> IO ()
iterNext it = do
    valid <- iterValid it
    when valid $ c_rocksdb_iter_next it

-- | Moves to the previous entry in the source. After this call, 'iterValid' is
-- /true/ iff the iterator was not positioned at the first entry in the source.
--
-- If the iterator is not valid, this function does nothing: RocksDB does not
-- allow moving an invalid iterator, so reposition it with 'iterSeek',
-- 'iterFirst' or 'iterLast' instead.
iterPrev :: Iterator -> IO ()
iterPrev it = do
    valid <- iterValid it
    when valid $ c_rocksdb_iter_prev it

-- | Return the key for the current entry if the iterator is currently
-- positioned at an entry, ie. 'iterValid'.
iterKey :: Iterator -> IO (Maybe ByteString)
iterKey it = iterString it c_rocksdb_iter_key

-- | Return the value for the current entry if the iterator is currently
-- positioned at an entry, ie. 'iterValid'.
iterValue :: Iterator -> IO (Maybe ByteString)
iterValue it = iterString it c_rocksdb_iter_value

-- | Return the current entry as a pair, if the iterator is currently positioned
-- at an entry, ie. 'iterValid'.
iterEntry :: Iterator -> IO (Maybe (ByteString, ByteString))
iterEntry it = do
    mkey <- iterKey it
    mval <- iterValue it
    return $ (,) <$> mkey <*> mval

-- | Check for errors
--
-- Note that this captures somewhat severe errors such as a corrupted database.
-- An iterator that becomes invalid because of an error looks the same as one
-- that reached the end of the data, so call this after iterating to tell them
-- apart. Returns 'Nothing' if there was no error.
iterGetError :: Iterator -> IO (Maybe ByteString)
iterGetError iter_ptr = alloca $ \err_ptr -> do
    poke err_ptr nullPtr
    c_rocksdb_iter_get_error iter_ptr err_ptr
    err_str <- peek err_ptr
    if err_str == nullPtr
        then return Nothing
        else Just <$> BU.unsafePackMallocCString err_str

--
-- Internal
--

-- | Read a C string from the iterator using the given accessor, if the
-- iterator is valid. The string is copied, since it belongs to the iterator.
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
