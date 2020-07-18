{-# LANGUAGE BinaryLiterals    #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.IO.Class       (MonadIO (liftIO))
import           Control.Monad.Trans.Resource (MonadResource, runResourceT)
import           Data.Default                 (def)
import           Database.RocksDB
import           System.IO.Temp               (withSystemTempDirectory)
import           Test.Hspec                   (describe, hspec, it,
                                               shouldReturn)
import           Test.QuickCheck              (Arbitrary (..),
                                               UnicodeString (..), generate)

initializeDB :: MonadResource m => FilePath -> m DB
initializeDB path = do
    opts <- newOptions def {createIfMissing = True}
    open path opts

main :: IO ()
main =  do
  hspec $ do
    describe "Basic DB Functionality" $ do
      it "should put items into the database and retrieve them" $  do
        runResourceT $ withSystemTempDirectory "rocksdb" $ \path -> do
          db <- initializeDB path
          put db "zzz" "zzz"
          get db "zzz"
        `shouldReturn` (Just "zzz")

      it "should put items into a database whose filepath has unicode characters and\
        \ retrieve them" $  do
        runResourceT $ withSystemTempDirectory "rocksdb" $ \path -> do
          unicode <- getUnicodeString <$> liftIO (generate arbitrary)
          db <- initializeDB $ path ++ unicode
          put db "zzz" "zzz"
          get db "zzz"
        `shouldReturn` (Just "zzz")
