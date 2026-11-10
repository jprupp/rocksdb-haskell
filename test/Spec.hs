{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Tests of "Database.RocksDB" against a real RocksDB, in temporary
-- directories.
--
-- There are two kinds of test here, because the bindings can go wrong in two
-- ways:
--
-- * Examples, which pin down one behaviour each: what a function returns, when
--   it throws, what a setting changes in the files RocksDB writes. Several
--   exist because a binding was once wrong without anything failing, so they
--   check an effect that can be observed rather than that a call returns.
--
-- * Properties, which run random sequences of writes against the database and
--   against a 'Map', and require that the two agree however they are read.
--   They find what nobody thought to write an example for: empty keys and
--   values, bytes that C would take for the end of a string, a delete of a key
--   that was never there, the order of iteration.
--
-- Every test opens its own database, so tests do not depend on each other or
-- on the order they run in.
module Main (main) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception (bracket)
import Control.Monad
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as C
import Data.Default (def)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe
import Database.RocksDB
import GHC.Clock (getMonotonicTime)
import System.IO.Temp
import Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import Test.QuickCheck hiding (Property)
import Test.QuickCheck qualified as QC
import Text.Printf

main :: IO ()
main = hspec $ do
  around (withTunedDB ["one", "two", "tree"]) tunedSpec
  around withPlainDB plainSpec
  boundSpec
  openingSpec
  tuningSpec
  modifyMaxSuccess (const 50) modelSpec

-------------------------------------------------------------------------------
-- Databases to test against
-------------------------------------------------------------------------------

-- | A configuration with a Bloom filter and a prefix extractor, as a program
-- tuned for point lookups would have. The older tests run against it.
tuned :: Config
tuned =
  def
    { createIfMissing = True,
      errorIfExists = True,
      bloomFilter = True,
      prefixLength = Just 3
    }

-- | The defaults, creating the database. Tests that iterate over the whole
-- database use it, since a prefix extractor changes what a seek promises.
plain :: Config
plain = def {createIfMissing = True}

-- | Run a test in a temporary directory that is deleted afterwards.
withTempPath :: (FilePath -> IO a) -> IO a
withTempPath = withSystemTempDirectory "rocksdb-tests"

-- | A new 'tuned' database with the given column families.
withTunedDB :: [String] -> (DB -> IO a) -> IO a
withTunedDB cfs go =
  withTempPath $ \path -> withDBCF path tuned (map (,tuned) cfs) go

-- | A new 'plain' database with one column family besides the default.
withPlainDB :: (DB -> IO a) -> IO a
withPlainDB go =
  withTempPath $ \path ->
    withDBCF path plain {createMissingColumnFamilies = True} [("other", plain)] go

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

-- | The first column family of a database opened with some.
firstFamily :: DB -> IO ColumnFamily
firstFamily db = case columnFamilies db of
  cf : _ -> return cf
  [] -> fail "the database has no column families"

-- | The column families of a database opened with exactly two.
twoFamilies :: DB -> IO (ColumnFamily, ColumnFamily)
twoFamilies db = case columnFamilies db of
  [a, b] -> return (a, b)
  cfs -> fail ("expected two column families, got " <> show (length cfs))

-- | A key that sorts by its number.
key :: Int -> ByteString
key = C.pack . printf "k%07d"

-- | The entries from where the iterator is until it stops being valid, moving
-- it with the given step: 'iterNext' to read forwards, 'iterPrev' backwards.
scan :: (Iterator -> IO ()) -> Iterator -> IO [(ByteString, ByteString)]
scan step itr = do
  valid <- iterValid itr
  if valid
    then do
      entry <- iterEntry itr
      step itr
      maybe id (:) entry <$> scan step itr
    else return []

-- | Every entry the iterator can reach, in ascending order of key. Fails if
-- the iterator stopped because of an error rather than at the end.
forwards :: Iterator -> IO [(ByteString, ByteString)]
forwards itr = do
  iterFirst itr
  entries <- scan iterNext itr
  iterGetError itr `shouldReturn` Nothing
  return entries

-- | Every entry the iterator can reach, in descending order of key.
backwards :: Iterator -> IO [(ByteString, ByteString)]
backwards itr = do
  iterLast itr
  entries <- scan iterPrev itr
  iterGetError itr `shouldReturn` Nothing
  return entries

-- | The time that so many runs of an action take, in seconds.
timed :: Int -> IO a -> IO Double
timed n action = do
  t0 <- getMonotonicTime
  replicateM_ n action
  t1 <- getMonotonicTime
  return (t1 - t0)

-- | Whether a property value is a number greater than zero.
positive :: Maybe ByteString -> Bool
positive = maybe False (maybe False ((> 0) . fst) . C.readInt)

-- | A number in the text of the @rocksdb.aggregated-table-properties@
-- property, which reads @name=value; name=value; ...@.
tableProperty :: ByteString -> ByteString -> Maybe Int
tableProperty name text =
  listToMaybe
    [ n
    | field <- C.split ';' text,
      let (k, v) = C.break (== '=') (C.dropWhile (== ' ') field),
      k == name,
      Just (n, _) <- [C.readInt (C.drop 1 v)]
    ]

-------------------------------------------------------------------------------
-- Examples against a tuned database
-------------------------------------------------------------------------------

boundSpec :: Spec
boundSpec = describe "The bound that ends a prefix" $ do
  it "gives the bound that ends a prefix" $ do
    prefixUpperBound "user:" `shouldBe` Just "user;"
    prefixUpperBound "a\xff\xff" `shouldBe` Just "b"
    prefixUpperBound "\xff\xff" `shouldBe` Nothing
    prefixUpperBound "" `shouldBe` Nothing
  prop "bounds exactly the keys with the prefix" $ \(ws, vs) ->
    let prefix = BS.pack ws
        rest = BS.pack vs
        inside b k = (prefix `BS.isPrefixOf` k) == (k >= prefix && k < b)
     in case prefixUpperBound prefix of
          Nothing -> BS.all (== 0xff) prefix
          -- a key that has the prefix, and one that most likely has not
          Just b -> inside b (prefix <> rest) && inside b rest

tunedSpec :: SpecWith DB
tunedSpec = do
  describe "Database" $ do
    it "puts and gets an item" $ \db -> do
      put db "aaa" "zzz"
      get db "aaa" `shouldReturn` Just "zzz"
    it "puts and gets from different column families" $ \db -> do
      two <- firstFamily db
      put db "aaa_key" "aaa_value"
      get db "aaa_key" `shouldReturn` Just "aaa_value"
      getCF db two "aaa_key" `shouldReturn` Nothing
      putCF db two "two_key" "two_value"
      getCF db two "two_key" `shouldReturn` Just "two_value"
      get db "two_key" `shouldReturn` Nothing
  describe "Multi-get" $ do
    it "reads many keys at once, in order, with missing ones" $ \db -> do
      one <- firstFamily db
      forM_ [1 .. 100 :: Int] $ \i -> do
        put db (C.pack (show i)) (C.pack ("v" <> show i))
        putCF db one (C.pack (show i)) (C.pack ("one" <> show i))
      let keys = map (C.pack . show) [50, 0, 1, 100, 101, 50 :: Int]
      multiGet db keys
        `shouldReturn` [Just "v50", Nothing, Just "v1", Just "v100", Nothing, Just "v50"]
      multiGetCF db one keys
        `shouldReturn` [Just "one50", Nothing, Just "one1", Just "one100", Nothing, Just "one50"]
      multiGet db [] `shouldReturn` []
      -- more keys than fit in one small buffer, most of them repeated
      let numbers = map (`mod` 150) [1 .. 2000 :: Int]
          expected i
            | i >= 1 && i <= 100 = Just (C.pack ("v" <> show i))
            | otherwise = Nothing
      multiGet db (map (C.pack . show) numbers) `shouldReturn` map expected numbers
  describe "Multithreading" $ do
    it "stores and retrieves items from multiple threads" $ \db -> do
      let val i = C.pack $ printf "val_%04d" i
          indices = [0 .. 9999] :: [Int]
      forConcurrently_ indices $ \i -> put db (key i) (val i)
      mapConcurrently (get db . key) indices `shouldReturn` map (Just . val) indices
  describe "Iterators on a tuned database" $ do
    it "retrieves entries written from many threads, in order" $ \db -> do
      let val i = C.pack $ printf "val_%03d" i
          indices = [0 .. 999] :: [Int]
          kvs = [(key i, val i) | i <- indices]
      forConcurrently_ kvs $ uncurry (put db)
      kvs' <- withIter db $ \itr -> do
        iterSeek itr (key 500)
        scan iterNext itr
      kvs' `shouldBe` drop 500 kvs
    it "seeks to the first key with a prefix" $ \db -> do
      forM_ ["aaa1", "aaa2", "bbb1", "bbb2", "ccc1"] $ \k -> put db k ("v" <> k)
      withIter db $ \itr -> do
        iterSeek itr "bbb"
        iterEntry itr `shouldReturn` Just ("bbb1", "vbbb1")
        iterNext itr
        iterEntry itr `shouldReturn` Just ("bbb2", "vbbb2")
  describe "Iterators and prefixes" $ do
    -- the prefix of this database is three bytes
    let keys = ["aaa1", "aaa2", "bbb1", "bbb2", "ccc1"] :: [ByteString]
        fill db = forM_ keys $ \k -> put db k k
        from k itr = iterSeek itr k >> map fst <$> scan iterNext itr
    it "stays within the prefix it was sent to" $ \db -> do
      fill db
      withIterConfig db Nothing def {prefixSameAsStart = True} $ \itr -> do
        from "bbb" itr `shouldReturn` ["bbb1", "bbb2"]
        iterGetError itr `shouldReturn` Nothing
        -- the same iterator, sent elsewhere, has its bound there
        from "aaa2" itr `shouldReturn` ["aaa2"]
        from "ccc" itr `shouldReturn` ["ccc1"]
        from "abc" itr `shouldReturn` []
    it "stays within the prefix when the data is in table files" $ \db -> do
      fill db
      -- enough writes to flush the keys out of memory, where the prefix
      -- filters are
      forM_ [0 .. 20000] $ \i -> put db ("zzz" <> key i) (BS.replicate 1000 0)
      withIterConfig db Nothing def {prefixSameAsStart = True} $ \itr -> do
        from "bbb" itr `shouldReturn` ["bbb1", "bbb2"]
        from "abc" itr `shouldReturn` []
    it "walks every prefix in order when asked for the total order" $ \db -> do
      fill db
      withIterConfig db Nothing def {totalOrderSeek = True} $ \itr -> do
        map fst <$> forwards itr `shouldReturn` keys
        map fst <$> backwards itr `shouldReturn` reverse keys
        from "abc" itr `shouldReturn` ["bbb1", "bbb2", "ccc1"]
    it "takes an upper bound across prefixes with the total order" $ \db -> do
      fill db
      let cfg = def {totalOrderSeek = True, upperBound = Just "bbb2"}
      withIterConfig db Nothing cfg (from "aaa2") `shouldReturn` ["aaa2", "bbb1"]

  describe "Properties" $ do
    it "reports memory use, per database and per column family" $ \db -> do
      one <- firstFamily db
      let isNumber = maybe False (\v -> not (C.null v) && C.all (`elem` ['0' .. '9']) v)
      forM_ [1 .. 1000] $ \i -> putCF db one (key i) "value"
      mem <- getPropertyCF db one CurSizeAllMemTables
      mem `shouldSatisfy` isNumber
      mem `shouldSatisfy` positive
      getProperty db EstimateTableReadersMem >>= (`shouldSatisfy` isNumber)
      getProperty db BlockCacheUsage >>= (`shouldSatisfy` isNumber)
      getProperty db (PropertyName "rocksdb.num-immutable-mem-table")
        >>= (`shouldSatisfy` isNumber)
      getProperty db (PropertyName "rocksdb.no-such-property")
        `shouldReturn` Nothing

-------------------------------------------------------------------------------
-- Examples against a plain database
-------------------------------------------------------------------------------

plainSpec :: SpecWith DB
plainSpec = do
  describe "Keys and values" $ do
    it "overwrites a value" $ \db -> do
      put db "k" "first"
      put db "k" "second"
      get db "k" `shouldReturn` Just "second"
    it "tells an empty value from an absent key" $ \db -> do
      put db "empty" ""
      get db "empty" `shouldReturn` Just ""
      get db "absent" `shouldReturn` Nothing
      multiGet db ["empty", "absent"] `shouldReturn` [Just "", Nothing]
    it "keeps bytes that C takes for the end of a string" $ \db -> do
      -- keys and values cross to C with their lengths, not as C strings
      let k = "a\0b"
          v = BS.pack [0, 255, 0, 10, 0]
      put db k v
      put db "a" "short"
      get db k `shouldReturn` Just v
      get db "a" `shouldReturn` Just "short"
      withIter db forwards `shouldReturn` [("a", "short"), (k, v)]
    it "stores a value of several megabytes" $ \db -> do
      let big = BS.replicate (4 * 1024 * 1024) 7
      put db "big" big
      get db "big" `shouldReturn` Just big
  describe "Deletes" $ do
    it "removes a key" $ \db -> do
      put db "k" "v"
      delete db "k"
      get db "k" `shouldReturn` Nothing
      withIter db forwards `shouldReturn` []
    it "accepts a key that is not there" $ \db -> do
      delete db "never written"
      get db "never written" `shouldReturn` Nothing
    it "removes a key from one column family only" $ \db -> do
      other <- firstFamily db
      put db "k" "default"
      putCF db other "k" "other"
      deleteCF db other "k"
      getCF db other "k" `shouldReturn` Nothing
      get db "k" `shouldReturn` Just "default"
      delete db "k"
      putCF db other "k" "again"
      get db "k" `shouldReturn` Nothing
      getCF db other "k" `shouldReturn` Just "again"
  describe "Batches" $ do
    it "applies puts and deletes across column families" $ \db -> do
      other <- firstFamily db
      put db "old" "v"
      putCF db other "old" "v"
      write
        db
        [ Put "a" "1",
          PutCF other "b" "2",
          Del "old",
          DelCF other "old"
        ]
      withIter db forwards `shouldReturn` [("a", "1")]
      withIterCF db other forwards `shouldReturn` [("b", "2")]
    it "applies operations in order" $ \db -> do
      write db [Put "k" "first", Put "k" "second", Put "gone" "v", Del "gone", Del "back", Put "back" "v"]
      withIter db forwards `shouldReturn` [("back", "v"), ("k", "second")]
    it "accepts an empty batch" $ \db -> do
      write db []
      withIter db forwards `shouldReturn` []
    it "writes a large batch" $ \db -> do
      let kvs = [(key i, C.pack (show i)) | i <- [1 .. 20000]]
      write db (map (uncurry Put) kvs)
      withIter db forwards `shouldReturn` kvs
  describe "Iterators" $ do
    it "is invalid on an empty database" $ \db ->
      withIter db $ \itr -> do
        iterFirst itr
        iterValid itr `shouldReturn` False
        iterLast itr
        iterValid itr `shouldReturn` False
        iterSeek itr "hello"
        iterKey itr `shouldReturn` Nothing
        iterValue itr `shouldReturn` Nothing
        iterEntry itr `shouldReturn` Nothing
        iterGetError itr `shouldReturn` Nothing
    it "walks back and forth" $ \db -> do
      put db "a" "aaa"
      put db "b" "bbb"
      put db "c" "ccc"
      withIter db $ \itr -> do
        iterSeek itr "b"
        iterEntry itr `shouldReturn` Just ("b", "bbb")
        iterNext itr
        iterEntry itr `shouldReturn` Just ("c", "ccc")
        iterNext itr -- after the last entry
        iterValid itr `shouldReturn` False
        iterKey itr `shouldReturn` Nothing
        iterValue itr `shouldReturn` Nothing
        iterPrev itr -- it cannot come back
        iterKey itr `shouldReturn` Nothing
        iterValue itr `shouldReturn` Nothing
      withIter db $ \itr -> do
        iterSeek itr "b"
        iterPrev itr
        iterEntry itr `shouldReturn` Just ("a", "aaa")
        iterPrev itr -- before the first entry
        iterValid itr `shouldReturn` False
        iterNext itr -- an invalid iterator stays invalid
        iterKey itr `shouldReturn` Nothing
        iterValue itr `shouldReturn` Nothing
    it "reads everything in both directions" $ \db -> do
      let kvs = [(key i, C.pack (show i)) | i <- [1 .. 100]]
      forM_ kvs $ uncurry (put db)
      withIter db forwards `shouldReturn` kvs
      withIter db backwards `shouldReturn` reverse kvs
    it "seeks to the next key when the one sought is absent" $ \db -> do
      forM_ [10, 20, 30] $ \i -> put db (key i) "v"
      withIter db $ \itr -> do
        iterSeek itr (key 15)
        iterKey itr `shouldReturn` Just (key 20)
        iterSeek itr (key 30)
        iterKey itr `shouldReturn` Just (key 30)
        iterSeek itr (key 31)
        iterValid itr `shouldReturn` False
    it "does not see writes made after it was created" $ \db -> do
      put db "a" "1"
      withIter db $ \itr -> do
        put db "b" "2"
        delete db "a"
        forwards itr `shouldReturn` [("a", "1")]
      withIter db forwards `shouldReturn` [("b", "2")]
    it "stays within its column family" $ \db -> do
      other <- firstFamily db
      put db "d" "default"
      putCF db other "o" "other"
      withIter db forwards `shouldReturn` [("d", "default")]
      withIterCF db other forwards `shouldReturn` [("o", "other")]
    it "can be created and destroyed by hand" $ \db -> do
      put db "a" "1"
      bracket (createIterator db Nothing) destroyIterator forwards
        `shouldReturn` [("a", "1")]
  describe "Bounded iterators" $ do
    let keys = ["a1", "a2", "b1", "b2", "c1", "c2"] :: [ByteString]
        fill db = forM_ keys $ \k -> put db k k
        from k itr = iterSeek itr k >> map fst <$> scan iterNext itr
    it "stops below an upper bound, which is outside" $ \db -> do
      fill db
      withIterConfig db Nothing def {upperBound = Just "b2"} $ \itr -> do
        from "a2" itr `shouldReturn` ["a2", "b1"]
        iterGetError itr `shouldReturn` Nothing
        map fst <$> backwards itr `shouldReturn` ["b1", "a2", "a1"]
        from "b2" itr `shouldReturn` []
    it "stops at a lower bound, which is inside" $ \db -> do
      fill db
      withIterConfig db Nothing def {lowerBound = Just "b1"} $ \itr -> do
        map fst <$> backwards itr `shouldReturn` ["c2", "c1", "b2", "b1"]
        map fst <$> forwards itr `shouldReturn` ["b1", "b2", "c1", "c2"]
        from "a1" itr `shouldReturn` ["b1", "b2", "c1", "c2"]
    it "keeps between two bounds" $ \db -> do
      fill db
      let cfg = def {lowerBound = Just "a2", upperBound = Just "c1"}
      withIterConfig db Nothing cfg $ \itr -> do
        map fst <$> forwards itr `shouldReturn` ["a2", "b1", "b2"]
        map fst <$> backwards itr `shouldReturn` ["b2", "b1", "a2"]
    it "scans a prefix with the bound that ends it" $ \db -> do
      fill db
      withIterConfig db Nothing def {upperBound = prefixUpperBound "b"} $ \itr ->
        from "b" itr `shouldReturn` ["b1", "b2"]
    it "returns nothing below an empty upper bound" $ \db -> do
      fill db
      withIterConfig db Nothing def {upperBound = Just ""} $ \itr ->
        forwards itr `shouldReturn` []
    it "is an ordinary iterator without bounds" $ \db -> do
      fill db
      withIterConfig db Nothing def $ \itr ->
        map fst <$> forwards itr `shouldReturn` keys
    it "keeps bounds with bytes that C takes for the end of a string" $ \db -> do
      let ks = ["k\0a", "k\0b", "k\0c"]
      forM_ ks $ \k -> put db k k
      withIterConfig db Nothing def {upperBound = Just "k\0c"} $ \itr ->
        map fst <$> forwards itr `shouldReturn` ["k\0a", "k\0b"]
    it "stays within its column family" $ \db -> do
      cf <- firstFamily db
      fill db
      putCF db cf "b3" "other"
      withIterConfig db (Just cf) def {upperBound = Just "c"} $ \itr ->
        forwards itr `shouldReturn` [("b3", "other")]
    it "reads from the snapshot of its database" $ \db -> do
      fill db
      withSnapshot db $ \snap -> do
        put db "b15" "late"
        delete db "b2"
        let cfg = def {upperBound = prefixUpperBound "b"}
        withIterConfig snap Nothing cfg (from "b") `shouldReturn` ["b1", "b2"]
        withIterConfig db Nothing cfg (from "b") `shouldReturn` ["b1", "b15"]
    it "can be created and destroyed by hand" $ \db -> do
      fill db
      (itr, destroyIt) <- createIteratorConfig db Nothing def {upperBound = Just "b"}
      from "a" itr `shouldReturn` ["a1", "a2"]
      destroyIt
    it "does not keep its bounds in memory that Haskell moves" $ \db -> do
      forM_ [0 .. 999] $ \i -> put db (key i) "v"
      -- the bound is built at run time, used by many iterators, and the
      -- heap is churned between seeks
      forM_ [1 .. 50 :: Int] $ \n ->
        withIterConfig db Nothing def {upperBound = Just (key (n * 10))} $ \itr -> do
          let junk = sum (map BS.length [key j | j <- [1 .. 20000]])
          junk `shouldBe` 160000
          iterSeek itr (key 0)
          length <$> scan iterNext itr `shouldReturn` n * 10
    it "does not step over deleted keys past its upper bound" $ \db -> do
      -- keys of "b" that are all deleted, between two keys that live: an
      -- unbounded scan of "a" reads every marker to find "c"
      let dead = [C.pack (printf "b%06d" i) | i <- [1 .. 20000 :: Int]]
      put db "a1" "v"
      put db "c1" "v"
      write db (map (\k -> Put k "v") dead)
      write db (map Del dead)
      let bounded = withIterConfig db Nothing def {upperBound = prefixUpperBound "a"} (from "a2")
          unbounded = withIter db $ \itr -> iterSeek itr "a2" >> iterKey itr
      bounded `shouldReturn` []
      unbounded `shouldReturn` Just "c1"
      slow <- timed 20 unbounded
      fast <- timed 20 bounded
      -- measured at 2,600 microseconds against 2.5; ten times leaves room
      -- for a busy machine
      (fast * 10 < slow) `shouldBe` True

  describe "Snapshots" $ do
    it "hides later writes and deletes from every kind of read" $ \db -> do
      other <- firstFamily db
      put db "a" "1"
      put db "b" "2"
      putCF db other "x" "100"
      withSnapshot db $ \snap -> do
        put db "a" "changed"
        delete db "b"
        put db "c" "3"
        putCF db other "x" "changed"
        get snap "a" `shouldReturn` Just "1"
        multiGet snap ["a", "b", "c"] `shouldReturn` [Just "1", Just "2", Nothing]
        getCF snap other "x" `shouldReturn` Just "100"
        withIter snap forwards `shouldReturn` [("a", "1"), ("b", "2")]
        withIterCF snap other forwards `shouldReturn` [("x", "100")]
        bracket (createIterator snap Nothing) destroyIterator forwards
          `shouldReturn` [("a", "1"), ("b", "2")]
      withIter db forwards `shouldReturn` [("a", "changed"), ("c", "3")]
    it "writes through a snapshot to the live database" $ \db ->
      withSnapshot db $ \snap -> do
        put snap "k" "v"
        get snap "k" `shouldReturn` Nothing
        get db "k" `shouldReturn` Just "v"
    it "agrees between point reads and iterators during concurrent writes" $ \db -> do
      cf <- firstFamily db
      let keys = ["key1", "key2", "key3"]
          original = [(k, "original") | k <- keys]
      forM_ original $ uncurry (putCF db cf)
      bracket (createSnapshot db) releaseSnapshot $ \(snap, _) -> do
        started <- newEmptyMVar
        let writer n = do
              forM_ keys $ \k -> putCF db cf k (C.pack ("v" <> show (n :: Int)))
              when (n == 0) $ putMVar started ()
              threadDelay 500
              writer (n + 1)
        withAsync (writer 0) $ \_ -> do
          takeMVar started
          forM_ original $ \(k, v) -> do
            getCF snap cf k `shouldReturn` Just v
            threadDelay 5000
          withIterCF snap cf forwards `shouldReturn` original
    it "pins iterators to a snapshot given to them" $ \db -> do
      other <- firstFamily db
      put db "a" "1"
      put db "b" "2"
      putCF db other "x" "100"
      bracket (createSnapshot db) releaseSnapshot $ \(_, snap) -> do
        put db "a" "modified"
        put db "c" "3"
        putCF db other "x" "modified"
        putCF db other "z" "300"
        -- the live handle, with the snapshot passed to the iterator
        withIterSnap db (Just snap) forwards `shouldReturn` [("a", "1"), ("b", "2")]
        withIterSnapCF db (Just snap) other forwards `shouldReturn` [("x", "100")]
        -- without one, they read the database as it is now
        withIterSnap db Nothing forwards
          `shouldReturn` [("a", "modified"), ("b", "2"), ("c", "3")]
        withIterSnapCF db Nothing other forwards
          `shouldReturn` [("x", "modified"), ("z", "300")]
    it "pins an iterator created by hand, which owns its read options" $ \db -> do
      put db "m" "10"
      put db "n" "20"
      bracket (createSnapshot db) releaseSnapshot $ \(_, snap) -> do
        put db "m" "changed"
        let create = createIteratorSnap db (Just snap) Nothing
            destroy' (itr, opts) = destroyIterator itr >> forM_ opts destroyReadOpts
        bracket create destroy' $ \(itr, opts) -> do
          opts `shouldSatisfy` isJust
          forwards itr `shouldReturn` [("m", "10"), ("n", "20")]
        bracket (createIteratorSnap db Nothing Nothing) destroy' $ \(itr, opts) -> do
          opts `shouldSatisfy` isNothing
          forwards itr `shouldReturn` [("m", "changed"), ("n", "20")]

-------------------------------------------------------------------------------
-- Opening and closing
-------------------------------------------------------------------------------

openingSpec :: Spec
openingSpec = describe "Opening" $ do
  it "keeps the data when the database is closed and opened again" $
    withTempPath $ \path -> do
      withDB path plain $ \db -> put db "key" "value" >> delete db "gone"
      withDB path plain $ \db -> do
        get db "key" `shouldReturn` Just "value"
        withIter db forwards `shouldReturn` [("key", "value")]
  it "throws if the database is missing and may not be created" $
    withTempPath $ \path ->
      withDB path def (\_ -> return ()) `shouldThrow` anyIOException
  it "throws if the database exists and must not" $
    withTempPath $ \path -> do
      withDB path plain $ \_ -> return ()
      withDB path plain {errorIfExists = True} (\_ -> return ())
        `shouldThrow` anyIOException
  it "throws if the database is already open" $
    withTempPath $ \path ->
      withDB path plain $ \_ ->
        withDB path plain (\_ -> return ()) `shouldThrow` anyIOException
  it "closes the database when the function throws" $
    withTempPath $ \path -> do
      withDB path plain (\db -> put db "k" "v" >> ioError (userError "stop"))
        `shouldThrow` anyIOException
      -- the lock is released, and what was written is there
      withDB path plain $ \db -> get db "k" `shouldReturn` Just "v"
  it "throws if a column family of the database is left out" $
    withTempPath $ \path -> do
      let create = plain {createMissingColumnFamilies = True}
      withDBCF path create [("one", plain)] $ \_ -> return ()
      withDB path plain (\_ -> return ()) `shouldThrow` anyIOException
  it "throws if a column family is missing and may not be created" $
    withTempPath $ \path -> do
      withDB path plain $ \_ -> return ()
      withDBCF path plain [("one", plain)] (\_ -> return ())
        `shouldThrow` anyIOException
  it "adds a column family to an existing database" $
    withTempPath $ \path -> do
      let create = plain {createMissingColumnFamilies = True}
      withDBCF path create [("one", plain)] $ \db -> do
        one <- firstFamily db
        put db "key" "value"
        putCF db one "key" "one"
      withDBCF path create [("one", plain), ("two", plain)] $ \db -> do
        (one, two) <- twoFamilies db
        get db "key" `shouldReturn` Just "value"
        getCF db one "key" `shouldReturn` Just "one"
        getCF db two "key" `shouldReturn` Nothing
        putCF db two "new" "cf"
        getCF db two "new" `shouldReturn` Just "cf"
  it "gives the column families in the order they were asked for" $
    withTempPath $ \path -> do
      let create = plain {createMissingColumnFamilies = True}
      withDBCF path create [("one", plain), ("two", plain)] $ \db -> do
        (one, two) <- twoFamilies db
        putCF db one "name" "one"
        putCF db two "name" "two"
      withDBCF path plain [("two", plain), ("one", plain)] $ \db -> do
        (two, one) <- twoFamilies db
        getCF db one "name" `shouldReturn` Just "one"
        getCF db two "name" `shouldReturn` Just "two"

-------------------------------------------------------------------------------
-- Settings, checked by their effect
-------------------------------------------------------------------------------

tuningSpec :: Spec
tuningSpec = describe "Tuning" $ do
  it "flushes small memtables and keeps the data" $ do
    let small =
          tuned
            { writeBufferSize = Just (64 * 1024),
              dbWriteBufferSize = Just (256 * 1024)
            }
        cfConf = small {dbWriteBufferSize = Nothing}
        value = C.replicate 100 'v'
    withTempPath $ \path ->
      withDBCF path small [("one", cfConf), ("two", cfConf)] $ \db -> do
        (one, two) <- twoFamilies db
        forM_ [1 .. 20000] $ \i -> do
          put db (key i) value
          putCF db one (key i) value
          putCF db two (key i) value
        get db (key 1) `shouldReturn` Just value
        getCF db one (key 10000) `shouldReturn` Just value
        getCF db two (key 20000) `shouldReturn` Just value
        -- the memtables are far smaller than the data, so it is in files
        files <- getProperty db SSTables
        files `shouldSatisfy` maybe False (not . C.null)
  it "builds Bloom filters for a family that asks for one, and only for it" $
    withTempPath $ \path -> do
      let open = plain {createMissingColumnFamilies = True}
          families = [("with", def {bloomFilter = True}), ("without", def)]
          tableProps = PropertyName "rocksdb.aggregated-table-properties"
          -- bytes of filter blocks in the table files of a family
          filters d cf =
            getPropertyCF d cf tableProps
              >>= maybe (fail "no table properties") (return . tableProperty "filter block size")
      withDBCF path open families $ \db -> do
        (with, without) <- twoFamilies db
        forM_ [1 .. 5000] $ \i -> do
          putCF db with (key i) "value"
          putCF db without (key i) "value"
      -- opening the database again moves what was written from the
      -- write-ahead log to table files, which is where filters are
      withDBCF path open families $ \db -> do
        (with, without) <- twoFamilies db
        -- ten bits for each of the 5000 keys is 6250 bytes
        filters db with >>= (`shouldSatisfy` maybe False (> 5000))
        filters db without `shouldReturn` Just 0
        getCF db with (key 2500) `shouldReturn` Just "value"
        getCF db with "absent" `shouldReturn` Nothing
  it "estimates the space used by a range of keys" $
    withTempPath $ \path -> do
      withDB path plain $ \db ->
        forM_ [1 .. 5000] $ \i -> put db (key i) (C.pack (printf "%0100d" i))
      -- sizes are those of table files; see the test of Bloom filters
      withDB path plain $ \db -> do
        approximateSize db (key 1, key 5000) >>= (`shouldSatisfy` (> 0))
        approximateSize db ("x", "y") `shouldReturn` 0
  it "shares one block cache, holding index and filter blocks" $ do
    let size = 8 * 1024 * 1024 :: Int
        cached =
          tuned
            { errorIfExists = False,
              blockCacheSize = Just size,
              cacheIndexAndFilterBlocks = True,
              writeBufferSize = Just (64 * 1024)
            }
        value = C.replicate 100 'v'
        capacity = PropertyName "rocksdb.block-cache-capacity"
    withTempPath $ \path -> do
      withDBCF path cached [("one", cached)] $ \db -> do
        one <- firstFamily db
        forM_ [1 .. 20000] $ \i -> putCF db one (key i) value
      withDBCF path cached [("one", cached)] $ \db -> do
        one <- firstFamily db
        getProperty db capacity `shouldReturn` Just (C.pack (show size))
        getPropertyCF db one capacity `shouldReturn` Just (C.pack (show size))
        multiGetCF db one (map key [1, 10000, 20000])
          `shouldReturn` replicate 3 (Just value)
        getProperty db BlockCacheUsage >>= (`shouldSatisfy` positive)
  it "opens with a bound on the write-ahead log" $
    withTempPath $ \path ->
      withDBCF path tuned {maxTotalWalSize = Just (1024 * 1024)} [("one", tuned)] $ \db -> do
        put db "key" "value"
        get db "key" `shouldReturn` Just "value"
  it "works with few open files and paranoid checks" $
    withTempPath $ \path -> do
      let strict =
            plain
              { maxFiles = Just 20,
                paranoidChecks = True,
                writeBufferSize = Just (64 * 1024)
              }
          kvs = [(key i, C.replicate 100 'v') | i <- [1 .. 20000]]
      withDB path strict $ \db -> forM_ kvs $ uncurry (put db)
      withDB path strict $ \db -> withIter db forwards `shouldReturn` kvs

-------------------------------------------------------------------------------
-- Properties: the database against a Map
-------------------------------------------------------------------------------

-- | A write to the default column family.
data Op
  = Set ByteString ByteString
  | Unset ByteString
  deriving (Show)

-- | Short keys over four bytes, so that random operations often meet on the
-- same key. The bytes are the ones a binding is most likely to mishandle: zero,
-- which ends a C string, and 255, which is negative as a signed @char@ and
-- must still sort last.
genKey :: Gen ByteString
genKey = do
  n <- choose (0, 3)
  BS.pack <$> vectorOf n (elements [0, 1, 97, 255])

-- | Values of any bytes, often empty.
genValue :: Gen ByteString
genValue = do
  n <- frequency [(1, return 0), (4, choose (1, 24))]
  BS.pack <$> vectorOf n arbitrary

genOps :: Gen [Op]
genOps = listOf $ frequency [(3, Set <$> genKey <*> genValue), (1, Unset <$> genKey)]

-- | Random operations, shrinking a failure to the fewest that still fail.
forAllOps :: (Testable p) => ([Op] -> p) -> QC.Property
forAllOps = forAllShrink genOps (shrinkList (const []))

opKey :: Op -> ByteString
opKey (Set k _) = k
opKey (Unset k) = k

-- | What the database should hold after the operations, in order.
model :: [Op] -> Map ByteString ByteString
model = foldl apply Map.empty
  where
    apply m (Set k v) = Map.insert k v m
    apply m (Unset k) = Map.delete k m

-- | Apply the operations one write at a time.
runOps :: DB -> [Op] -> IO ()
runOps db = mapM_ run
  where
    run (Set k v) = put db k v
    run (Unset k) = delete db k

-- | The operations as one atomic batch.
batchOf :: [Op] -> [BatchOp]
batchOf = map op
  where
    op (Set k v) = Put k v
    op (Unset k) = Del k

-- | Require that every way of reading the default column family agrees with
-- the map: iteration in both directions, in the order of the map's keys, and
-- point reads of the given keys.
shouldHold :: DB -> [ByteString] -> Map ByteString ByteString -> IO ()
shouldHold db keys m = do
  withIter db forwards `shouldReturn` Map.toAscList m
  withIter db backwards `shouldReturn` Map.toDescList m
  forM_ keys $ \k -> get db k `shouldReturn` Map.lookup k m
  multiGet db keys `shouldReturn` map (`Map.lookup` m) keys

-- | A new database with only the default column family, for one test case.
inNewDB :: (DB -> IO ()) -> QC.Property
inNewDB go = ioProperty $ withTempPath $ \path -> withDB path plain go

modelSpec :: Spec
modelSpec = describe "Agreement with a Map" $ do
  prop "after single writes" $
    forAllOps $ \ops -> inNewDB $ \db -> do
      runOps db ops
      shouldHold db (map opKey ops) (model ops)
  prop "after one batch" $
    forAllOps $ \ops -> inNewDB $ \db -> do
      write db (batchOf ops)
      shouldHold db (map opKey ops) (model ops)
  prop "after the database is closed and opened again" $
    forAllOps $ \ops -> ioProperty $ withTempPath $ \path -> do
      withDB path plain $ \db -> runOps db ops
      withDB path plain $ \db -> shouldHold db (map opKey ops) (model ops)
  prop "in a column family, leaving the default one alone" $
    forAllOps $ \ops -> forAllOps $ \others -> ioProperty $ withPlainDB $ \db -> do
      other <- firstFamily db
      runOps db ops
      forM_ others $ \o -> case o of
        Set k v -> putCF db other k v
        Unset k -> deleteCF db other k
      shouldHold db (map opKey (ops <> others)) (model ops)
      withIterCF db other forwards `shouldReturn` Map.toAscList (model others)
      let keys = map opKey (ops <> others)
      multiGetCF db other keys `shouldReturn` map (`Map.lookup` model others) keys
  prop "when seeking: the first key not below the one sought" $
    forAllOps $ \ops -> forAll (listOf genKey) $ \targets -> inNewDB $ \db -> do
      runOps db ops
      withIter db $ \itr -> forM_ targets $ \target -> do
        iterSeek itr target
        iterEntry itr `shouldReturn` Map.lookupGE target (model ops)
  prop "through a snapshot, whatever is written afterwards" $
    forAllOps $ \earlier -> forAllOps $ \later -> inNewDB $ \db -> do
      let keys = map opKey (earlier <> later)
      runOps db earlier
      withSnapshot db $ \snap -> do
        runOps db later
        shouldHold snap keys (model earlier)
      shouldHold db keys (model (earlier <> later))
