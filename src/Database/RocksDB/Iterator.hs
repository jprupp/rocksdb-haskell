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
--   in a database or column family that has a 'prefixLength' and no key
--   shorter than it.
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
-- = Reading many entries in one call
--
-- 'iterNext' and 'iterPrev' may wait for the disk, so they are foreign calls
-- of the kind that lets other Haskell threads run meanwhile. That has a
-- price: the thread gives up its capability for the call and has to get one
-- back afterwards, and when more threads want to run than there are
-- capabilities, it waits for it. A scan that moves one step at a time pays
-- that for every entry.
--
-- The run-time system also walks the stack of the thread at every such call.
-- A loop that is not tail recursive, such as one that ends in
-- @(entry :) \<$\> go@, leaves a frame there for every entry read so far, so
-- each step costs more the deeper the scan is.
--
-- 'iterChunk' walks the iterator in C instead, and returns up to some
-- hundreds of entries from one call. 'iterEntries', 'iterEntriesBack',
-- 'iterEntriesPrefix' and 'iterEntriesPrefixBack' are short forms of it for
-- a known number of entries.
--
-- @
-- -- the same scan again, in chunks
-- usersFrom db = 'withIter' db $ \it -> do
--     'iterSeek' it "user:"
--     let go acc = do
--             (entries, more) <- 'iterChunk' it def{'chunkPrefix' = "user:"}
--             let acc' = entries : acc
--             if more then go acc' else return (concat (reverse acc'))
--     go []
-- @
--
-- Seconds for every thread to read 200,000 entries of 52 bytes, on four
-- capabilities and four cores, from the benchmark of this package:
--
-- > scan                                 1 thread   16 threads
-- > single steps, not tail recursive       0.78        3.9
-- > single steps, tail recursive           0.090       0.50
-- > chunks of 256 entries                  0.048       0.25
--
-- So a scan in chunks takes half the time of the best loop of single steps,
-- and a sixteenth of the time of a loop that is not tail recursive. On a
-- machine with more cores than capabilities it gains more, since the threads
-- that are inside the call do not hold a capability: with twenty more cores
-- free, the sixteen threads took 0.12 seconds in chunks and 0.83 in steps.
--
-- Use chunks to read a range. Keep to single steps where each move depends
-- on the entry just read, or where one entry is all that is wanted:
-- 'iterEntry' after a seek reads it with no call that moves the iterator.
--
-- A chunk also reports errors by itself: 'iterChunk' throws if the iterator
-- stopped at one, where single steps need 'iterGetError' to be asked.
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

    -- * Reading many entries in one call
    iterEntries,
    iterEntriesBack,
    iterEntriesPrefix,
    iterEntriesPrefixBack,
    ChunkConfig (..),
    iterChunk,
  )
where

import Control.Exception (bracket, finally, mask, mask_, onException, throwIO)
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
import Foreign.C.Types (CChar, CSize)

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
withIterSnap ::
  DB ->
  Maybe Snapshot ->
  (Iterator -> IO a) ->
  IO a
withIterSnap db msnap = withIterCommon db msnap Nothing

-- | Create 'Iterator' on a column family with a specific snapshot.
--
-- If 'Nothing' is passed for snapshot, the iterator creates its own implicit snapshot.
-- If 'Just snapshot' is passed, the iterator uses the provided snapshot.
withIterSnapCF ::
  DB ->
  Maybe Snapshot ->
  ColumnFamily ->
  (Iterator -> IO a) ->
  IO a
withIterSnapCF db msnap cf = withIterCommon db msnap (Just cf)

-- | Create an iterator over the default column family, or over a column family
-- if one is given, reading from a snapshot if one is given.
withIterCommon ::
  DB ->
  Maybe Snapshot ->
  Maybe ColumnFamily ->
  (Iterator -> IO a) ->
  IO a
withIterCommon DB {rocksDB = rocks_db, readOpts = read_opts} msnap mcf f =
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
--   the ranges have to be whole prefixes of that length, and no key may be
--   shorter than it.
-- * 'totalOrderSeek' when a database with a 'prefixLength' has to be walked
--   from end to end.
data IterConfig = IterConfig
  { -- | the smallest key that the iterator can be at. Going backwards, it
    -- becomes invalid instead of moving to a smaller key; a seek to a
    -- smaller key lands on the first key at or after the bound.
    lowerBound :: !(Maybe ByteString),
    -- | a key that the iterator stays below: the bound itself is outside.
    -- Going forwards, the iterator becomes invalid instead of reaching the
    -- bound or a larger key, and 'iterLast' goes to the last key below it.
    -- 'prefixUpperBound' gives the bound that ends a prefix.
    upperBound :: !(Maybe ByteString),
    -- | stay within the prefix of the key that the iterator was last sent
    -- to: it becomes invalid instead of moving to a key with another
    -- prefix. The prefix is the first 'Database.RocksDB.prefixLength'
    -- bytes of a key. After 'iterSeek' it is the prefix of the key
    -- sought; after 'iterFirst' or 'iterLast', that of the key found, so
    -- that those two give the first or the last prefix of the database
    -- and no more. It has no effect in a database or column family
    -- without a prefix length, nor together with 'totalOrderSeek'.
    --
    -- __Every key has to be at least as long as the prefix__: the keys
    -- sought, and the keys stored. RocksDB takes the prefix of a key
    -- without looking at its length, so with a shorter key it reads the
    -- bytes that follow it in memory, and where the iterator stops is
    -- then anyone's guess. This binding cannot check it for you, because
    -- an 'Iterator' does not know how it was made. If keys can be
    -- shorter, use an 'upperBound', which has no such condition.
    prefixSameAsStart :: !Bool,
    -- | see every key in order, whatever its prefix, in a database or
    -- column family that has a 'Database.RocksDB.prefixLength'. There, a
    -- plain iterator is only sure to return the keys that share the prefix
    -- of the key it was sent to; what it does once it moves past them, or
    -- after 'iterFirst', is not defined. That is the price of the prefix
    -- filters, which let a seek skip the files that have nothing for a
    -- prefix. This option gives up the filters and gets the order back.
    totalOrderSeek :: !Bool
  }
  deriving (Eq, Show)

-- | No bounds, and seeks as RocksDB does them by default.
instance Default IterConfig where
  def =
    IterConfig
      { lowerBound = Nothing,
        upperBound = Nothing,
        prefixSameAsStart = False,
        totalOrderSeek = False
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
createIterator DB {rocksDB = rocks_db, readOpts = read_opts} mcf =
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
createIteratorSnap ::
  DB ->
  Maybe Snapshot ->
  Maybe ColumnFamily ->
  IO (Iterator, Maybe ReadOpts)
createIteratorSnap db msnap mcf = case msnap of
  Nothing -> (,Nothing) <$> createIterator db mcf
  Just snap -> do
    ro <- c_rocksdb_readoptions_create
    c_rocksdb_readoptions_set_snapshot ro snap
    it <- createIteratorWithOpts db ro mcf
    return (it, Just ro)

-- | Internal: create iterator with explicit ReadOpts.
createIteratorWithOpts ::
  DB ->
  ReadOpts ->
  Maybe ColumnFamily ->
  IO Iterator
createIteratorWithOpts DB {rocksDB = rocks_db} ro mcf =
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

-- | The next entries, at most as many as asked for, from the one the
-- iterator is on and going forwards. The iterator is left on the entry after
-- the last one returned, so that calling again continues the scan.
--
-- Fewer entries than asked for means that the iterator reached the end, of
-- the data or of its bounds, and is no longer valid. A count of zero or less
-- returns nothing and does not move the iterator.
--
-- It returns what that many rounds of 'iterEntry' and 'iterNext' would, from
-- one foreign call for all of them; see the module header for what that
-- saves. Every entry is held in memory at once, twice for a moment, so ask
-- for no more than fit: to read a range of unknown size, call it in a loop,
-- or use 'iterChunk', which also takes a limit in bytes.
--
-- Throws an 'IOError' if the iterator stopped because of an error.
iterEntries :: Iterator -> Int -> IO [(ByteString, ByteString)]
iterEntries it n = fst <$> iterChunk it whole {chunkEntries = n}

-- | Like 'iterEntries', going backwards: the entry that the iterator is on
-- comes first and those before it follow, in descending order. The iterator
-- is left on the entry before the last one returned.
iterEntriesBack :: Iterator -> Int -> IO [(ByteString, ByteString)]
iterEntriesBack it n = fst <$> iterChunk it whole {chunkEntries = n, chunkBackwards = True}

-- | Like 'iterEntries', for the keys that start with a prefix: it also stops
-- at the first entry whose key does not, and leaves the iterator on that
-- entry, which is not returned. Fewer entries than asked for means that the
-- keys with the prefix have all been read.
--
-- It does not seek: send the iterator to the prefix with 'iterSeek' first.
--
-- @
-- -- the first hundred users
-- 'iterSeek' it "user:"
-- users <- 'iterEntriesPrefix' it "user:" 100
-- @
iterEntriesPrefix :: Iterator -> ByteString -> Int -> IO [(ByteString, ByteString)]
iterEntriesPrefix it prefix n =
  fst <$> iterChunk it whole {chunkEntries = n, chunkPrefix = prefix}

-- | Like 'iterEntriesPrefix', going backwards as 'iterEntriesBack' does. The
-- iterator has to be on the last key of the prefix to begin with: 'iterLast'
-- puts it there if its 'upperBound' is the 'prefixUpperBound' of the prefix.
iterEntriesPrefixBack :: Iterator -> ByteString -> Int -> IO [(ByteString, ByteString)]
iterEntriesPrefixBack it prefix n =
  fst <$> iterChunk it whole {chunkEntries = n, chunkPrefix = prefix, chunkBackwards = True}

-- | A chunk with no limit on its bytes and no prefix, for the functions that
-- promise a number of entries.
whole :: ChunkConfig
whole = def {chunkBytes = maxBound}

-- | What one call of 'iterChunk' reads: how much, which keys, and in which
-- direction. 'def' reads forwards, any key, up to 256 entries or 1 MiB;
-- override the fields you need:
--
-- @
-- def { 'chunkPrefix' = "user:" }
-- @
--
-- The two limits are there for different reasons. 'chunkEntries' spreads
-- the cost of the foreign call over many entries. In the benchmark of this
-- package, chunks of 16 take up to a fifth longer than chunks of 256, and chunks
-- of 1024 are no faster; a larger chunk only makes a reader that stops early
-- pay for more entries that it does not want. 'chunkBytes' bounds the
-- memory: a chunk is held twice for a moment, in the buffer that C fills and
-- in the 'ByteString's copied from it, and 256 values can be of any size.
data ChunkConfig = ChunkConfig
  { -- | the most entries to read. Zero or less reads nothing.
    chunkEntries :: !Int,
    -- | stop before the keys and values read take more bytes than this. The
    -- first entry is read whatever its size, so a chunk is only empty where
    -- there is nothing to read, and a scan gets past an entry that is
    -- larger than the limit.
    chunkBytes :: !Int,
    -- | read only entries whose key starts with these bytes, and stop at the
    -- first one that does not. Empty, the default, for any key.
    --
    -- The iterator still has to move to that first key to see that it is
    -- outside, as in a loop written by hand, and that can be slow where
    -- deleted keys follow the range: see the module header. An 'upperBound'
    -- or 'prefixSameAsStart' stops the iterator before it, and this prefix
    -- is then not needed, though it does no harm.
    chunkPrefix :: !ByteString,
    -- | go backwards, with 'iterPrev', and return the entries in descending
    -- order.
    chunkBackwards :: !Bool
  }
  deriving (Eq, Show)

-- | Forwards, any key, up to 256 entries or 1 MiB of keys and values.
instance Default ChunkConfig where
  def =
    ChunkConfig
      { chunkEntries = 256,
        chunkBytes = 1024 * 1024,
        chunkPrefix = B.empty,
        chunkBackwards = False
      }

-- | Read the next entries, as a t'ChunkConfig' says, from the one the
-- iterator is on, and tell whether more follow.
--
-- This is the function to build a scan on. The iterator is walked in C, in
-- one foreign call for the whole chunk, where a loop of 'iterEntry' and
-- 'iterNext' makes several for every entry; the module header has the
-- figures.
--
-- @
-- -- every entry whose key starts with "user:"
-- 'iterSeek' it "user:"
-- let go acc = do
--         (entries, more) <- 'iterChunk' it def{'chunkPrefix' = "user:"}
--         let acc' = entries : acc
--         if more then go acc' else return (concat (reverse acc'))
-- go []
-- @
--
-- The chunk ends at the first of: the end of the data; a bound of the
-- iterator, from 'upperBound', 'lowerBound' or 'prefixSameAsStart', which
-- all work as they do with 'iterNext' and 'iterPrev'; a key that does not
-- start with 'chunkPrefix'; 'chunkEntries' entries; 'chunkBytes' bytes.
--
-- __Where the iterator is left:__ on the first entry that was not returned,
-- or not valid if there is none. So the next call continues where this one
-- stopped, 'iterEntry' shows the entry that comes next, and after a chunk
-- that a prefix ended, the iterator is on the first key outside the prefix.
--
-- __The flag__ is 'True' if the iterator is left on an entry whose key
-- starts with 'chunkPrefix': the next call would return something. It is
-- 'False' when the scan is over, which saves the call that would find that
-- out by returning nothing.
--
-- The entries are ordinary 'ByteString's, each a copy of its own that the
-- garbage collector frees on its own.
--
-- Unlike the functions that move one step, this one throws an 'IOError' if
-- the iterator stopped because of an error, such as a corrupt block or a
-- failed read: there is no need to ask 'iterGetError' after it, and a scan
-- cannot take an error for the end of the data. It also throws if there is
-- no memory for a single entry.
iterChunk :: Iterator -> ChunkConfig -> IO ([(ByteString, ByteString)], Bool)
iterChunk it cfg =
  BU.unsafeUseAsCStringLen (chunkPrefix cfg) $ \(prefix_ptr, prefix_len) ->
    alloca $ \buffer_ptr ->
      alloca $ \count_ptr ->
        alloca $ \more_ptr ->
          -- Nothing may interrupt between the call that allocates the
          -- buffer and the handler that frees it.
          mask $ \restore -> do
            poke buffer_ptr nullPtr
            status <-
              throwIfErr "iterChunk" $
                c_rocksdb_haskell_iter_chunk
                  it
                  (if chunkBackwards cfg then 1 else 0)
                  (limit (chunkEntries cfg))
                  (limit (chunkBytes cfg))
                  prefix_ptr
                  (intToCSize prefix_len)
                  buffer_ptr
                  count_ptr
                  more_ptr
            buffer <- peek buffer_ptr
            flip finally (free buffer) $ do
              when (status /= 0) $
                throwIO (userError "iterChunk: no memory for an entry")
              count <- peek count_ptr
              more <- peek more_ptr
              entries <- restore (unpackChunk buffer (cSizeToInt count))
              return (entries, more /= 0)
  where
    -- a limit below zero is a limit of zero, and not a huge unsigned number
    limit = intToCSize . max 0

--
-- Internal
--

-- | Copy so many entries out of a buffer filled by
-- 'c_rocksdb_haskell_iter_chunk', in the order they are in. Each entry is
-- two lengths, the key, the value, and padding up to the size of a length;
-- see @cbits\/rocksdb_haskell.h@.
--
-- Every key and value becomes a 'ByteString' of its own. Slices of the one
-- buffer would save the copy, and keep the whole chunk alive for as long as
-- any one key of it is.
unpackChunk :: Ptr CChar -> Int -> IO [(ByteString, ByteString)]
unpackChunk buffer = go [] 0
  where
    word = sizeOf (0 :: CSize)
    go acc _ 0 = return (reverse acc)
    go acc offset n = do
      key_len <- cSizeToInt <$> peekByteOff buffer offset
      value_len <- cSizeToInt <$> peekByteOff buffer (offset + word)
      let key_ptr = buffer `plusPtr` (offset + 2 * word)
          value_ptr = key_ptr `plusPtr` key_len
          size = 2 * word + key_len + value_len
          padded = (size + word - 1) `div` word * word
      key <- B.packCStringLen (key_ptr, key_len)
      value <- B.packCStringLen (value_ptr, value_len)
      go ((key, value) : acc) (offset + padded) (n - 1 :: Int)

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
