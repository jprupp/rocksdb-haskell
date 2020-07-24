{-# LANGUAGE BinaryLiterals    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Main where

import           Data.Default            (def)
import           Data.String.Conversions
import           Database.RocksDB
import           Test.Hspec              (describe, hspec, it, shouldReturn)
import           UnliftIO

conf :: Config
conf = def { createIfMissing = True
           , errorIfExists   = True
           , bloomFilter     = True
           , prefixLength    = Just 3
           }

withTestDB :: MonadUnliftIO m => FilePath -> (DB -> m a) -> m a
withTestDB path =
    withDB path conf

withTestDBCF :: MonadUnliftIO m => FilePath -> [String] -> (DB -> m a) -> m a
withTestDBCF path cfs =
    withDBCF path conf (map (,conf) cfs)

main :: IO ()
main =  do
    hspec $ do
        describe "Database engine" $ do
            it "should put items into the database and retrieve them" $
                withSystemTempDirectory "rocksdb1" $ \path ->
                withTestDB path $ \db -> do
                    put db "zzz" "zzz"
                    get db "zzz" `shouldReturn` Just "zzz"
            it "should store and retrieve items from different column families" $
                withSystemTempDirectory "rocksdbcf1" $ \path ->
                withTestDBCF path ["two"] $ \db -> do
                    let [two] = columnFamilies db
                    put db "one" "one"
                    get db "one" `shouldReturn` Just "one"
                    getCF db two "one" `shouldReturn` Nothing
                    putCF db two "two" "two"
                    getCF db two "two" `shouldReturn` Just "two"
                    get db "two" `shouldReturn` Nothing
        describe "Multiple concurrent threads" $ do
            it "should be able to put and retrieve items" $
                withSystemTempDirectory "rocksdb2" $ \path ->
                withTestDB path $ \db -> do
                    let str = cs . show
                        key i = "key" <> str i
                        val i = "val" <> str i
                        indices = [1..500] :: [Int]
                        keys = map key indices
                        vals = map val indices
                        kvs = zip keys vals
                    as1 <- mapM (\(k, v) -> async $ put db k v) kvs
                    mapM_ wait as1
                    as2 <- mapM (\k -> async $ get db k) keys
                    mapM wait as2 `shouldReturn` map Just vals
