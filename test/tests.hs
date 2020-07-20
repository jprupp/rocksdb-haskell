{-# LANGUAGE BinaryLiterals    #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Default     (def)
import           Database.RocksDB
import           Test.Hspec       (describe, hspec, it, shouldReturn)
import           UnliftIO

data TestDB = TestDB { testDB :: !DB
                     , testReadOpts :: !ReadOpts
                     , testWriteOpts :: !WriteOpts
                     }

withTestDB :: MonadUnliftIO m
           => FilePath
           -> (TestDB -> m a)
           -> m a
withTestDB path f =
    withOptions def {createIfMissing = True} $ \opts ->
    withDB path opts $ \db ->
    withReadOpts Nothing $ \read_opts ->
    withWriteOpts $ \write_opts ->
    f TestDB { testDB = db
             , testReadOpts = read_opts
             , testWriteOpts = write_opts
             }

main :: IO ()
main =  do
    hspec $ do
        describe "Database engine" $ do
            it "should put items into the database and retrieve them" $
                withSystemTempDirectory "rocksdb" $ \path ->
                withTestDB path $ \db -> do
                    put (testDB db) (testWriteOpts db) "zzz" "zzz"
                    get (testDB db) (testReadOpts db) "zzz"
                        `shouldReturn` Just "zzz"
