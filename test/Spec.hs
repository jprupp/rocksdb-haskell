{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import Data.ByteString.Char8 qualified as C
import Data.Default (def)
import Data.Maybe
import Database.RocksDB
import System.IO.Temp
import Test.Hspec
import Text.Printf

conf :: Config
conf =
  def
    { createIfMissing = True,
      errorIfExists = True,
      bloomFilter = True,
      prefixLength = Just 3
    }

withTestDBCF :: [String] -> (DB -> IO a) -> IO a
withTestDBCF cfs go =
  withSystemTempDirectory "rocksdb-tests-cf" $ \path ->
    withDBCF path conf (map (,conf) cfs) go

main :: IO ()
main = do
  hspec $ around (withTestDBCF ["one", "two", "tree"]) $ do
    describe "Database" $ do
      it "puts and gets an item" $ \db -> do
        put db "aaa" "zzz"
        get db "aaa" `shouldReturn` Just "zzz"
      it "puts and gets from different type families" $ \db -> do
        let two = head $ columnFamilies db
        put db "aaa_key" "aaa_value"
        get db "aaa_key" `shouldReturn` Just "aaa_value"
        getCF db two "aaa_key" `shouldReturn` Nothing
        putCF db two "two_key" "two_value"
        getCF db two "two_key" `shouldReturn` Just "two_value"
        get db "two_key" `shouldReturn` Nothing
    describe "Multi-get" $ do
      it "reads many keys at once, in order, with missing ones" $ \db -> do
        let one = head (columnFamilies db)
        forM_ [1 .. 100 :: Int] $ \i -> do
          put db (C.pack (show i)) (C.pack ("v" <> show i))
          putCF db one (C.pack (show i)) (C.pack ("one" <> show i))
        let keys = map (C.pack . show) [50, 0, 1, 100, 101, 50 :: Int]
        multiGet db keys
          `shouldReturn` [Just "v50", Nothing, Just "v1", Just "v100", Nothing, Just "v50"]
        multiGetCF db one keys
          `shouldReturn` [Just "one50", Nothing, Just "one1", Just "one100", Nothing, Just "one50"]
        multiGet db [] `shouldReturn` []
        ks <- forM [1 .. 2000 :: Int] $ \i -> return (C.pack (show (i `mod` 150)))
        multiGet db ks >>= (`shouldBe` map (\k -> if read (C.unpack k) `elem` [1 .. 100 :: Int] then Just ("v" <> k) else Nothing) ks)
    describe "Multithreading" $ do
      it "stores and retrieve items from multiple threads" $ \db -> do
        let key i = C.pack $ printf "key_%04d" i
            val i = C.pack $ printf "val_%04d" i
            indices = [0 .. 9999] :: [Int]
            keys = map key indices
            vals = map val indices
            kvs = zip keys vals
        was <- mapM (\(k, v) -> async $ put db k v) kvs
        mapM_ wait was
        ras <- mapM (async . get db) keys
        mapM wait ras `shouldReturn` map Just vals
    describe "Iterators" $ do
      it "retrieves entries using iterators" $ \db -> do
        let key i = C.pack $ printf "key_%03d" i
            val i = C.pack $ printf "val_%03d" i
            indices = [0 .. 999] :: [Int]
            keys = map key indices
            vals = map val indices
            kvs = zip keys vals
        was <- mapM (\(k, v) -> async $ put db k v) kvs
        mapM_ wait was
        kvs' <- withIter db $ \itr -> do
          let start = keys !! 500
          iterSeek itr start
          fmap catMaybes $ replicateM 500 $ do
            mkv <- iterEntry itr
            iterNext itr
            return mkv
        kvs' `shouldBe` drop 500 kvs
      it "walks back and forth" $ \db -> do
        withIter db $ \itr -> do
          iterSeek itr "hello"
          iterKey itr `shouldReturn` Nothing
          iterValue itr `shouldReturn` Nothing
          iterEntry itr `shouldReturn` Nothing
        put db "a" "aaa"
        put db "b" "bbb"
        put db "c" "ccc"
        withIter db $ \itr -> do
          iterSeek itr "b"
          iterKey itr `shouldReturn` Just "b"
          iterValue itr `shouldReturn` Just "bbb"
          iterNext itr
          iterKey itr `shouldReturn` Just "c"
          iterValue itr `shouldReturn` Just "ccc"
          iterNext itr -- After the last entry
          iterKey itr `shouldReturn` Nothing
          iterValue itr `shouldReturn` Nothing
          iterPrev itr -- It can't come back
          iterKey itr `shouldReturn` Nothing
          iterValue itr `shouldReturn` Nothing
        withIter db $ \itr -> do
          iterSeek itr "b"
          iterKey itr `shouldReturn` Just "b"
          iterValue itr `shouldReturn` Just "bbb"
          iterPrev itr
          iterKey itr `shouldReturn` Just "a"
          iterValue itr `shouldReturn` Just "aaa"
          iterPrev itr -- Invalid before lowest key
          iterKey itr `shouldReturn` Nothing
          iterValue itr `shouldReturn` Nothing
          iterNext itr -- Invalid iterator stays invalid
          iterKey itr `shouldReturn` Nothing
          iterValue itr `shouldReturn` Nothing
    describe "Snapshots" $ do
      it "getCF and iterator see same data during concurrent writes" $ \db -> do
        let cf = head $ columnFamilies db
        -- Initial data
        putCF db cf "key1" "val1"
        putCF db cf "key2" "val2"
        putCF db cf "key3" "val3"

        -- Create snapshot
        (snapDB, snap) <- createSnapshot db

        -- Start writer thread modifying the same keys
        started <- newEmptyMVar
        writerThread <- forkIO $ do
          putMVar started ()
          let loop n
                | n > 100 = pure ()
                | otherwise = do
                    let suffix = C.pack $ "-v" <> show n
                    putCF db cf "key1" ("val1" <> suffix)
                    putCF db cf "key2" ("val2" <> suffix)
                    putCF db cf "key3" ("val3" <> suffix)
                    threadDelay 500
                    loop (n + 1 :: Int)
          loop (0 :: Int)

        takeMVar started
        threadDelay 1000 -- Let writer get going

        -- Read via getCF on snapshot
        v1 <- getCF snapDB cf "key1"
        threadDelay 10000
        v2 <- getCF snapDB cf "key2"
        threadDelay 10000
        v3 <- getCF snapDB cf "key3"

        -- Read via iterator on same snapshot (using the same column family)
        iterEntries <- withIterCF snapDB cf $ \itr -> do
          iterFirst itr
          let collect acc = do
                valid <- iterValid itr
                if valid
                  then do
                    mentry <- iterEntry itr
                    iterNext itr
                    collect (mentry : acc)
                  else pure (reverse acc)
          catMaybes <$> collect []

        -- Clean up
        killThread writerThread
        releaseSnapshot (snapDB, snap)

        -- Verify: getCF and iterator see the same original values
        v1 `shouldBe` Just "val1"
        v2 `shouldBe` Just "val2"
        v3 `shouldBe` Just "val3"
        iterEntries `shouldBe` [("key1", "val1"), ("key2", "val2"), ("key3", "val3")]

      it "withIterSnap sees snapshot data" $ \db -> do
        put db "a" "1"
        put db "b" "2"
        (_, snap) <- createSnapshot db
        -- Modify after snapshot
        put db "a" "modified"
        put db "c" "3"
        -- Iterator with explicit snapshot should see original data
        entries <- withIterSnap db (Just snap) $ \itr -> do
          iterFirst itr
          let collect acc = do
                valid <- iterValid itr
                if valid
                  then do
                    mentry <- iterEntry itr
                    iterNext itr
                    collect (mentry : acc)
                  else pure (reverse acc)
          catMaybes <$> collect []
        entries `shouldBe` [("a", "1"), ("b", "2")]

      it "withIterSnapCF sees snapshot data on column family" $ \db -> do
        let cf = head $ columnFamilies db
        putCF db cf "x" "100"
        putCF db cf "y" "200"
        (_, snap) <- createSnapshot db
        -- Modify after snapshot
        putCF db cf "x" "modified"
        putCF db cf "z" "300"
        -- Iterator with explicit snapshot on CF
        entries <- withIterSnapCF db (Just snap) cf $ \itr -> do
          iterFirst itr
          let collect acc = do
                valid <- iterValid itr
                if valid
                  then do
                    mentry <- iterEntry itr
                    iterNext itr
                    collect (mentry : acc)
                  else pure (reverse acc)
          catMaybes <$> collect []
        entries `shouldBe` [("x", "100"), ("y", "200")]

      it "createIteratorSnap with manual management" $ \db -> do
        put db "m" "10"
        put db "n" "20"
        (_, snap) <- createSnapshot db
        put db "m" "changed"
        -- Manual iterator creation with snapshot
        (itr, mReadOpts) <- createIteratorSnap db (Just snap) Nothing
        iterFirst itr
        k1 <- iterKey itr
        v1 <- iterValue itr
        iterNext itr
        k2 <- iterKey itr
        v2 <- iterValue itr
        destroyIterator itr
        forM_ mReadOpts destroyReadOpts
        (k1, v1) `shouldBe` (Just "m", Just "10")
        (k2, v2) `shouldBe` (Just "n", Just "20")
    describe "Write buffers" $ do
      it "flushes small memtables and keeps the data" $ \_ -> do
        let small =
              conf
                { writeBufferSize = Just (64 * 1024),
                  dbWriteBufferSize = Just (256 * 1024)
                }
            cfConf = small {dbWriteBufferSize = Nothing}
            key :: Int -> C.ByteString
            key i = C.pack (printf "k%07d" i)
            value = C.replicate 100 'v'
        withSystemTempDirectory "rocksdb-tests-wb" $ \path ->
          withDBCF path small [("one", cfConf), ("two", cfConf)] $ \db -> do
            let [one, two] = columnFamilies db
            forM_ [1 .. 20000] $ \i -> do
              put db (key i) value
              putCF db one (key i) value
              putCF db two (key i) value
            get db (key 1) `shouldReturn` Just value
            getCF db one (key 10000) `shouldReturn` Just value
            getCF db two (key 20000) `shouldReturn` Just value
            files <- getProperty db SSTables
            files `shouldSatisfy` maybe False (not . C.null)
    describe "Properties" $ do
      it "reports memory use, per database and per column family" $ \db -> do
        let one = head (columnFamilies db)
            isNumber = maybe False (\v -> not (C.null v) && C.all (`elem` ['0' .. '9']) v)
        forM_ [1 .. 1000 :: Int] $ \i -> putCF db one (C.pack (show i)) "value"
        mem <- getPropertyCF db one CurSizeAllMemTables
        mem `shouldSatisfy` isNumber
        mem `shouldSatisfy` maybe False ((> 0) . read . C.unpack)
        getProperty db EstimateTableReadersMem >>= (`shouldSatisfy` isNumber)
        getProperty db BlockCacheUsage >>= (`shouldSatisfy` isNumber)
        getProperty db (PropertyName "rocksdb.num-immutable-mem-table")
          >>= (`shouldSatisfy` isNumber)
        getProperty db (PropertyName "rocksdb.no-such-property")
          `shouldReturn` Nothing
      it "shares one block cache, holding index and filter blocks" $ \_ -> do
        let size = 8 * 1024 * 1024
            cached =
              conf
                { errorIfExists = False,
                  blockCacheSize = Just size,
                  cacheIndexAndFilterBlocks = True,
                  writeBufferSize = Just (64 * 1024)
                }
            key :: Int -> C.ByteString
            key i = C.pack (printf "k%07d" i)
            value = C.replicate 100 'v'
            capacity d cf =
              maybe (getProperty d p) (\c -> getPropertyCF d c p) cf
              where
                p = PropertyName "rocksdb.block-cache-capacity"
        withSystemTempDirectory "rocksdb-tests-cache" $ \path -> do
          withDBCF path cached [("one", cached)] $ \db -> do
            let one = head (columnFamilies db)
            forM_ [1 .. 20000] $ \i -> putCF db one (key i) value
          withDBCF path cached [("one", cached)] $ \db -> do
            let one = head (columnFamilies db)
            capacity db Nothing `shouldReturn` Just (C.pack (show size))
            capacity db (Just one) `shouldReturn` Just (C.pack (show size))
            multiGetCF db one (map key [1, 10000, 20000])
              `shouldReturn` replicate 3 (Just value)
            usage <- getProperty db BlockCacheUsage
            usage `shouldSatisfy` maybe False ((> 0) . (read :: String -> Int) . C.unpack)
      it "opens with a bound on the write-ahead log" $ \_ ->
        withSystemTempDirectory "rocksdb-tests-wal" $ \path ->
          withDBCF path conf {maxTotalWalSize = Just (1024 * 1024)} [("one", conf)] $ \db -> do
            put db "key" "value"
            get db "key" `shouldReturn` Just "value"
    describe "Column families" $ do
      it "adds a column family to an existing database" $ \_ ->
        withSystemTempDirectory "rocksdb-tests-newcf" $ \path -> do
          let open = conf {errorIfExists = False}
          withDBCF path open [("one", open)] $ \db -> put db "key" "value"
          withDBCF path open {createMissingColumnFamilies = True} [("one", open), ("two", open)] $ \db -> do
            get db "key" `shouldReturn` Just "value"
            let two = columnFamilies db !! 1
            putCF db two "new" "cf"
            getCF db two "new" `shouldReturn` Just "cf"
