{-# LANGUAGE BinaryLiterals    #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Default     (def)
import           Database.RocksDB
import           Test.Hspec       (describe, hspec, it, shouldReturn)
import           UnliftIO

withTestDB :: MonadUnliftIO m => FilePath -> (DB -> m a) -> m a
withTestDB path = withDB path def{createIfMissing = True}

main :: IO ()
main =  do
    hspec $ do
        describe "Database engine" $ do
            it "should put items into the database and retrieve them" $
                withSystemTempDirectory "rocksdb" $ \path ->
                withTestDB path $ \db -> do
                    put db "zzz" "zzz"
                    get db "zzz" `shouldReturn` Just "zzz"
