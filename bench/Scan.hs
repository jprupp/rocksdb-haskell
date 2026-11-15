{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- What a scan costs with one foreign call for each entry, and with one for
-- each chunk of entries.
--
-- The question it answers: a Haskell program pays for every foreign call
-- that may block, since the thread gives up its capability for the call and
-- has to get one back afterwards. How much of the time of a scan is that,
-- and how much is left when 'iterChunk' makes the call once for a few
-- hundred entries?
--
-- The answer depends on how many threads want a capability. A thread alone
-- gets its own back at once. Sixteen threads on four capabilities, as in a
-- server under load, each find theirs taken when the call returns, and wait.
-- So every scan is timed with one thread and with sixteen, on four
-- capabilities, whatever the machine has: the suite is built with @-N4@.
--
-- It also times the loop that is not tail recursive, @(entry :) \<$\> go@,
-- because the run-time system looks at the stack of a thread at every such
-- call, and that loop leaves a frame on it for every entry read so far.
--
-- Each scan reads every entry of a database of 'entryCount' small entries
-- from a table file, and adds up their sizes. Run it with @stack bench@; to
-- run some of the scans only, name them: @stack bench --ba chunks@. The figures of
-- the CHANGELOG and of the README come from it.
module Main (main) where

import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.Async (forConcurrently)
import Control.Monad (forM, forM_, unless)
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as C
import Data.Default (def)
import Data.List (isInfixOf, sort)
import Database.RocksDB
import GHC.Clock (getMonotonicTime)
import GHC.Conc (getNumProcessors)
import System.Environment (getArgs)
import System.IO.Temp (withSystemTempDirectory)
import System.Mem (performGC)
import Text.Printf (printf)

-- | Entries in the database, all of which every scan reads.
entryCount :: Int
entryCount = 200000

-- | Times each measurement is taken. The median is reported: other programs
-- on the machine make single runs slow, never fast.
runs :: Int
runs = 5

-- | A key of 20 bytes that sorts by its number.
key :: Int -> ByteString
key = C.pack . printf "key:%016d"

-- | A value of 32 bytes: about what an index entry holds.
value :: Int -> ByteString
value = C.pack . printf "value:%026d"

-- | A way to read everything from where the iterator is, giving the number
-- of entries and the bytes of their keys and values. No scan keeps the
-- entries: what a program does with them, and what the garbage collector
-- then has to do, is not what is measured here.
type Scan = Iterator -> IO Total

-- | Entries read, and the bytes of their keys and values.
data Total = Total !Int !Int
  deriving (Eq, Show)

-- | Count an entry.
add :: Total -> (ByteString, ByteString) -> Total
add (Total n bytes) (k, v) = Total (n + 1) (bytes + B.length k + B.length v)

-- | One step for each entry, in a loop that keeps its total in an argument:
-- the stack stays as it is.
steps :: Scan
steps it = go (Total 0 0)
  where
    go !total =
      iterEntry it >>= \case
        Just entry -> iterNext it >> go (add total entry)
        Nothing -> return total

-- | One step for each entry, in a loop that is not tail recursive, as one
-- that returns a list with @(entry :) \<$\> go@ is: each entry waits on the
-- stack for those that follow.
stepsOnStack :: Scan
stepsOnStack it = go
  where
    go =
      iterEntry it >>= \case
        Just entry -> do
          iterNext it
          !total <- go
          return (add total entry)
        Nothing -> return (Total 0 0)

-- | Chunks of so many entries.
chunks :: Int -> Scan
chunks n it = go (Total 0 0)
  where
    go !total = do
      (entries, more) <- iterChunk it def {chunkEntries = n}
      let total' = foldl' add total entries
      if more then go total' else return total'

-- | The seconds that so many threads take to scan the whole database at the
-- same time, each with its own iterator.
timeScan :: DB -> Scan -> Int -> IO Double
timeScan db scan threads = do
  performGC
  t0 <- getMonotonicTime
  totals <- forConcurrently [1 .. threads] $ \_ ->
    withIter db $ \it -> iterFirst it >> scan it
  t1 <- getMonotonicTime
  -- every key is of 20 bytes and every value of 32
  unless (all (== Total entryCount (entryCount * 52)) totals) $
    fail ("a scan read " <> show totals <> ", and not every entry")
  return (t1 - t0)

-- | The median of 'runs' measurements.
median :: IO Double -> IO Double
median measure = do
  times <- forM [1 .. runs] (const measure)
  return (sort times !! (runs `div` 2))

main :: IO ()
main = withSystemTempDirectory "rocksdb-bench" $ \dir ->
  withDB dir def {createIfMissing = True} $ \db -> do
    forM_ [0, 1000 .. entryCount - 1] $ \i ->
      write db [Put (key j) (value j) | j <- [i .. min entryCount (i + 1000) - 1]]
    -- Compacted, the entries are in a table file, where those of a database
    -- in production are, and not in the memtable.
    compactRange db Nothing Nothing Nothing
    caps <- getNumCapabilities
    cores <- getNumProcessors
    printf "%d entries, %d capabilities, %d cores, median of %d runs\n" entryCount caps cores runs
    printf "%-34s %8s %10s %18s\n" (s "scan") (s "threads") (s "seconds") (s "entries a second")
    -- once unmeasured, to fill the block cache
    _ <- timeScan db (chunks 256) 1
    wanted <- getArgs
    let chosen = [x | x@(name, _) <- scans, null wanted || any (`isInfixOf` name) wanted]
    forM_ chosen $ \(name, scan) ->
      forM_ [1, 16] $ \threads -> do
        seconds <- median (timeScan db scan threads)
        let rate = fromIntegral (threads * entryCount) / seconds :: Double
        printf "%-34s %8d %10.3f %18.0f\n" name threads seconds rate
  where
    s :: String -> String
    s = id
    scans :: [(String, Scan)]
    scans =
      [ ("single steps, not tail recursive", stepsOnStack),
        ("single steps", steps),
        ("chunks of 16", chunks 16),
        ("chunks of 64", chunks 64),
        ("chunks of 256", chunks 256),
        ("chunks of 1024", chunks 1024)
      ]
