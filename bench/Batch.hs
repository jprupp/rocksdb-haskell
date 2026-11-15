{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- What it costs to hand a large batch to RocksDB with one foreign call for
-- each operation, and with one for each chunk of packed operations.
--
-- The question it answers: 'write' gives RocksDB the operations of a batch
-- one call at a time, and a Haskell program pays for every foreign call
-- that may block, since the thread gives up its capability for the call and
-- has to get one back afterwards. How much of the time of a large batch is
-- that, and how much is left when 'writePacked' makes the call once for a
-- few thousand operations?
--
-- A batch is 'operationCount' operations over four column families, about
-- what the import of a Bitcoin block writes. It is written three ways:
--
-- * with 'write', from a list of t'BatchOp';
--
-- * with 'writePacked', from the same list packed by 'packOps' into chunks
--   of 'chunkOperations', the packing included in the time;
--
-- * with 'writePacked', from chunks that were packed beforehand: what is
--   left for a program that serialises its keys and values straight into
--   chunks, and so has no list to pack.
--
-- All three end with the same @rocksdb_write@, which adds the operations to
-- the memtables and takes most of the time. The difference between the
-- rows is what the hand-over costs.
--
-- The answer depends on whether other threads want the capabilities. A
-- thread alone gets its own back at once after a call. In a server under
-- load it finds it taken, and waits. So every way is timed alone and next
-- to 'busyThreads' threads that compute, on four capabilities whatever the
-- machine has: the suite is built with @-N4@.
--
-- Run it with @stack bench rocksdb-haskell-jprupp:bench:batch@, and on
-- four cores with @taskset -c 0-3@ before it. The figures of the CHANGELOG
-- and of the README come from it.
module Main (main) where

import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.Async (withAsync)
import Control.Exception (evaluate)
import Control.Monad (forM, forM_, unless)
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as C
import Data.Default (def)
import Data.IORef (atomicModifyIORef', newIORef)
import Data.List (sort)
import Database.RocksDB
import GHC.Clock (getMonotonicTime)
import GHC.Conc (getNumProcessors)
import System.IO.Temp (withSystemTempDirectory)
import System.Mem (performGC)
import Text.Printf (printf)

-- | Operations in a batch.
operationCount :: Int
operationCount = 65000

-- | Operations in a chunk of a packed batch: with keys of 34 bytes, values
-- of 66 and the header of a record, a chunk of about 256 KiB.
chunkOperations :: Int
chunkOperations = 2400

-- | Times each measurement is taken. The median is reported: other programs
-- on the machine make single runs slow, never fast.
runs :: Int
runs = 15

-- | Threads that compute while a batch is written, in the second
-- measurement of each way.
busyThreads :: Int
busyThreads = 8

-- | A key of 34 bytes that is new in every batch: the number of the batch
-- and that of the operation.
key :: Int -> Int -> ByteString
key batch i = C.pack (printf "key:%010d:%019d" batch i)

-- | A value of 66 bytes.
value :: Int -> ByteString
value = C.pack . printf "value:%060d"

-- | The operations of a batch: one in eight deletes a key, the others set
-- one, in the four column families in turn.
operations :: [ColumnFamily] -> Int -> [BatchOp]
operations families batch = zipWith op (cycle families) [1 .. operationCount]
  where
    op cf i
      | i `mod` 8 == 0 = DelCF cf (key batch i)
      | otherwise = PutCF cf (key batch i) (value i)

-- | A list in pieces of a length, the last one shorter.
groupsOf :: Int -> [a] -> [[a]]
groupsOf _ [] = []
groupsOf n xs = let (a, b) = splitAt n xs in a : groupsOf n b

-- | A way to write the operations of a batch: what has to be ready before
-- the clock starts, and the write that is timed.
data Way a = Way
  { name :: String,
    prepare :: [BatchOp] -> IO a,
    timed :: DB -> a -> IO ()
  }

-- | Every operation with its key and value in memory.
ready :: [BatchOp] -> IO [BatchOp]
ready ops = do
  forM_ ops $ \op -> evaluate $ case op of
    Put k v -> B.length k + B.length v
    PutCF _ k v -> B.length k + B.length v
    Del k -> B.length k
    DelCF _ k -> B.length k
  return ops

-- | The seconds that a way takes to write a batch of new keys.
timeWrite :: DB -> IO Int -> Way a -> IO Double
timeWrite db next way = do
  batch <- next
  input <- prepare way =<< ready (operations (columnFamilies db) batch)
  performGC
  t0 <- getMonotonicTime
  timed way db input
  t1 <- getMonotonicTime
  return (t1 - t0)

-- | Run an action next to so many threads that compute and allocate, as
-- the threads of a server do, and stop them afterwards.
besideBusy :: Int -> IO a -> IO a
besideBusy 0 action = action
besideBusy n action = withAsync (spin 0) (const (besideBusy (n - 1) action))
  where
    -- it allocates, so that it can be stopped, and depends on its
    -- argument, so that it is not worked out once and for all
    spin :: Int -> IO ()
    spin i = do
      _ <- evaluate (sum (map fromEnum (show [i .. i + 2000])))
      spin (i + 1)

-- | The median of 'runs' measurements.
median :: IO Double -> IO Double
median measure = do
  times <- forM [1 .. runs] (const measure)
  return (sort times !! (runs `div` 2))

main :: IO ()
main = withSystemTempDirectory "rocksdb-bench" $ \dir -> do
  let config = def {createIfMissing = True, createMissingColumnFamilies = True}
      families = [(n, config) | n <- ["one", "two", "three", "four"]]
  withDBCF dir config families $ \db -> do
    caps <- getNumCapabilities
    cores <- getNumProcessors
    counter <- newIORef 0
    let next = atomicModifyIORef' counter (\n -> (n + 1, n))
        row :: Way a -> IO ()
        row way = do
          alone <- median (timeWrite db next way)
          busy <- median (besideBusy busyThreads (timeWrite db next way))
          printf "%-44s %10.1f %10.1f\n" (name way) (1000 * alone) (1000 * busy)
    printf "batches of %d operations, about %d bytes of keys and values\n" operationCount size
    printf "%d capabilities, %d cores, median of %d runs, milliseconds a batch\n" caps cores runs
    printf "%-44s %10s %10s\n" (s "") (s "alone") (s (show busyThreads <> " busy"))
    -- once unmeasured, so that the first row does not pay for the start
    _ <- timeWrite db next oneCall
    unless (caps == 4) $ putStrLn "note: the figures of the documentation are for four capabilities"
    row oneCall
    row packedList
    row packedAhead
  where
    s :: String -> String
    s = id
    -- one operation in eight is a delete, with a key of 34 bytes and no
    -- value of 66
    size = operationCount * 100 - operationCount `div` 8 * 66
    oneCall = Way "write: a call for each operation" return write
    packedList =
      Way "writePacked, packOps included" return $ \db ops ->
        writePacked db (map packOps (groupsOf chunkOperations ops))
    packedAhead =
      Way "writePacked, chunks packed beforehand" pack writePacked
    pack ops = do
      let chunks = map packOps (groupsOf chunkOperations ops)
      forM_ chunks (evaluate . B.length)
      return chunks
