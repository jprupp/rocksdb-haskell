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
--
-- = Why iterators
--
-- Sorted keys are what make RocksDB more than a hash table. Keys that share a
-- prefix are stored next to each other, so all the records of one user, or all
-- the transactions of one address, can be read with one seek and a short scan.
-- Designing keys so that the records you read together sort together is the
-- most important performance decision in a RocksDB schema.
--
-- = Bounds
--
-- An 'Iterator' from 'withIter' has no end but that of the data. The scan
-- above stops when it /reads/ a key that does not start with @user:@, so
-- RocksDB has to produce that key first. Usually it is the next one and costs
-- nothing. It is not when the keys after the range have been deleted: a
-- deleted key stays in the database, as a marker, until compaction drops it,
-- and an iterator steps over every marker on its way to the next key that is
-- alive. A scan of an empty range that is followed by ten thousand deleted
-- keys reads ten thousand markers to return nothing.
--
-- 'withIterConfig' creates an iterator that RocksDB itself stops:
--
-- * at an 'upperBound' or a 'lowerBound', which are keys;
-- * at the end of the prefix that it was sent to, with 'prefixSameAsStart',
--   in a database or column family that has a 'prefixLength'.
--
-- Such an iterator becomes invalid at its bound without looking past it. A
-- bound also lets RocksDB leave out the table files that hold nothing inside
-- it.
--
-- @
-- -- the same scan, which ends where the keys of "user:" end
-- usersFrom db =
--     'withIterConfig' db Nothing def{'upperBound' = 'prefixUpperBound' "user:"} $ \\it -> do
--         'iterSeek' it "user:"
--         let go acc = 'iterEntry' it >>= \\case
--                 Just kv -> 'iterNext' it >> go (kv : acc)
--                 Nothing -> return (reverse acc)
--         go []
-- @
--
-- = Safety
--
-- The C API crashes the process if an invalid iterator is moved, and the bytes
-- it returns are only valid until it moves again. This module guards against
-- both: 'iterNext' and 'iterPrev' do nothing on an invalid iterator, and keys
-- and values are copied into Haskell memory before they are returned.
module Database.RocksDB.Iterator
  ( Iterator,
    -- * Bracket-style iteration
    withIter,
    withIterCF,
    withIterSnap,
    withIterSnapCF,
    -- * Bounded iteration
    IterConfig (..),
    withIterConfig,
    prefixUpperBound,
    -- * Manual iterator management
    createIterator,
    createIteratorSnap,
    createIteratorConfig,
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

import Control.Exception (bracket, mask_, onException)
import Control.Monad (forM, forM_, when)
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Unsafe qualified as BU
import Data.Default (Default (..))
import Database.RocksDB.C
import Database.RocksDB.Internal
import Foreign
import Foreign.C.Error (throwErrnoIfNull)
import Foreign.C.String (CString)
import Foreign.C.Types (CSize)

-- | Create 'Iterator' and use it.
--
-- Note that an 'Iterator' creates a snapshot of the database implicitly, so
-- updates written after the iterator was created are not visible. To read
-- from an older 'Snapshot' instead, create the iterator from the t'DB' that
-- 'Database.RocksDB.withSnapshot' provides: its 'readOpts' carry the snapshot.
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

-- | Where an iterator stops, and what a seek promises. 'def' asks for
-- nothing, which gives the iterator that 'withIter' creates; override the
-- fields you need:
--
-- @
-- def { 'upperBound' = 'prefixUpperBound' "user:" }
-- @
--
-- The fields are the read options of RocksDB that concern iterators and that
-- change what a scan costs or returns. They are a record of their own, and
-- not arguments, so that more of them can be added without breaking callers.
--
-- Which one to use:
--
-- * 'upperBound' when each range gets an iterator of its own. It needs no
--   configuration of the database and works with keys of any shape.
-- * 'prefixSameAsStart' when one iterator is sent to many ranges, one seek
--   after another: the bound moves with each seek. It needs a 'prefixLength',
--   and the ranges have to be whole prefixes of that length.
-- * 'totalOrderSeek' when a database with a 'prefixLength' has to be walked
--   from end to end.
data IterConfig = IterConfig
    { lowerBound :: !(Maybe ByteString)
      -- ^ the smallest key that the iterator can be at. Going backwards, it
      -- becomes invalid instead of moving to a smaller key; a seek to a
      -- smaller key lands on the first key at or after the bound.
    , upperBound :: !(Maybe ByteString)
      -- ^ a key that the iterator stays below: the bound itself is outside.
      -- Going forwards, the iterator becomes invalid instead of reaching the
      -- bound or a larger key, and 'iterLast' goes to the last key below it.
      -- 'prefixUpperBound' gives the bound that ends a prefix.
    , prefixSameAsStart :: !Bool
      -- ^ stay within the prefix of the key that the iterator was last sent
      -- to with 'iterSeek': the iterator becomes invalid instead of moving
      -- to a key with another prefix. The prefix is the first
      -- 'Database.RocksDB.prefixLength' bytes of a key. It has no effect in
      -- a database or column family without a prefix length, nor together
      -- with 'totalOrderSeek', nor after 'iterFirst' or 'iterLast', which
      -- name no prefix.
    , totalOrderSeek :: !Bool
      -- ^ see every key in order, whatever its prefix, in a database or
      -- column family that has a 'Database.RocksDB.prefixLength'. There, a
      -- plain iterator is only sure to return the keys that share the prefix
      -- of the key it was sent to; what it does once it moves past them, or
      -- after 'iterFirst', is not defined. That is the price of the prefix
      -- filters, which let a seek skip the files that have nothing for a
      -- prefix. This option gives up the filters and gets the order back.
    }
    deriving (Eq, Show)

-- | No bounds, and seeks as RocksDB does them by default.
instance Default IterConfig where
    def =
        IterConfig
            { lowerBound = Nothing
            , upperBound = Nothing
            , prefixSameAsStart = False
            , totalOrderSeek = False
            }

-- | The smallest key that is greater than every key that starts with the
-- given prefix: the 'upperBound' of a scan of the prefix.
--
-- It is the prefix with its last byte raised by one, after dropping the bytes
-- @0xff@ at its end, which cannot be raised. An empty prefix, or one that is
-- all @0xff@, has no key above its keys, and gives 'Nothing': the scan needs
-- no bound, since it ends where the data ends.
--
-- >>> prefixUpperBound "user:"
-- Just "user;"
prefixUpperBound :: ByteString -> Maybe ByteString
prefixUpperBound prefix =
    case B.unsnoc (B.dropWhileEnd (== 0xff) prefix) of
        Nothing -> Nothing
        Just (start, byte) -> Just (B.snoc start (byte + 1))

-- | Create an 'Iterator' that stops where an t'IterConfig' says, over the
-- default column family or over a column family if one is given, and use it.
--
-- Like 'withIter', it reads from the snapshot of the t'DB' if it comes from
-- 'Database.RocksDB.withSnapshot', and otherwise sees the database as it was
-- when the iterator was created.
--
-- Use it instead of 'withIter' when the range to scan is known beforehand
-- and what follows it may hold many deleted keys, as in a queue or in any
-- index whose entries come and go: see the module header. For a range that
-- is followed by live keys there is little to gain. Creating the iterator
-- costs slightly more than with 'withIter', since it gets read options of
-- its own and the bounds are copied.
--
-- Iterator should not be used after computation ends.
withIterConfig :: DB -> Maybe ColumnFamily -> IterConfig -> (Iterator -> IO a) -> IO a
withIterConfig db mcf cfg f =
    bracket (createIteratorConfig db mcf cfg) snd (f . fst)

-- | Manually create an unmanaged iterator like that of 'withIterConfig'.
--
-- It returns the iterator and the action that destroys it. Run the action
-- once, when done with the iterator, and before the t'DB' is closed or its
-- snapshot released. Do not use 'destroyIterator' on this iterator: RocksDB
-- keeps pointers to the bounds for as long as the iterator lives, so they
-- are in memory that only that action frees, along with the read options.
--
-- For automatic resource management, prefer 'withIterConfig'.
createIteratorConfig :: DB -> Maybe ColumnFamily -> IterConfig -> IO (Iterator, IO ())
createIteratorConfig db mcf cfg = mask_ $ do
    ro <- c_rocksdb_readoptions_create
    lower <- bound ro c_rocksdb_readoptions_set_iterate_lower_bound (lowerBound cfg)
    upper <- bound ro c_rocksdb_readoptions_set_iterate_upper_bound (upperBound cfg)
    let release = do
            c_rocksdb_readoptions_destroy ro
            mapM_ free lower
            mapM_ free upper
    it <- flip onException release $ do
        forM_ (snapshot db) (c_rocksdb_readoptions_set_snapshot ro)
        when (prefixSameAsStart cfg) $ c_rocksdb_readoptions_set_prefix_same_as_start ro 1
        when (totalOrderSeek cfg) $ c_rocksdb_readoptions_set_total_order_seek ro 1
        createIteratorWithOpts db ro mcf
    return (it, c_rocksdb_iter_destroy it >> release)
  where
    -- A copy of the bound outside the Haskell heap, where it does not move
    -- and lives until it is freed. One byte is asked for at least, so that
    -- an empty bound has a pointer too.
    bound ::
        ReadOpts ->
        (ReadOpts -> CString -> CSize -> IO ()) ->
        Maybe ByteString ->
        IO (Maybe CString)
    bound ro set mkey = forM mkey $ \key -> do
        let len = B.length key
        ptr <- mallocBytes (max 1 len)
        BU.unsafeUseAsCString key $ \src -> copyBytes ptr src len
        set ro ptr (intToCSize len)
        return ptr

-- | Manually create unmanaged iterator, over the default column family or over
-- a column family if one is given. It uses the 'readOpts' of the t'DB', so if
-- the t'DB' comes from 'Database.RocksDB.withSnapshot' it reads from that
-- snapshot. It must be released with 'destroyIterator', and must not outlive
-- the t'DB'.
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
--
-- The check costs one cheap call and turns a crash in C into a harmless no-op,
-- which is the right trade for a Haskell binding.
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
--
-- The key is copied, because RocksDB reuses the memory behind it as soon as the
-- iterator moves.
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
