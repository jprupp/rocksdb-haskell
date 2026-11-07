# rocksdb-haskell-jprupp

[![Hackage](https://img.shields.io/hackage/v/rocksdb-haskell-jprupp.svg)](https://hackage.haskell.org/package/rocksdb-haskell-jprupp)
[![License: BSD-3-Clause](https://img.shields.io/badge/license-BSD--3--Clause-blue.svg)](https://github.com/jprupp/rocksdb-haskell/blob/master/LICENSE)

Haskell bindings to [RocksDB](https://rocksdb.org), the embedded persistent
key-value store. The package on Hackage is `rocksdb-haskell-jprupp`; the module
to import is `Database.RocksDB`.

The bindings follow the RocksDB C API closely. On top of it they add
bracket-style resource management, so that databases, snapshots and iterators
are always released, even when an exception is thrown.

- [API documentation](https://hackage.haskell.org/package/rocksdb-haskell-jprupp)
- [Changelog](https://github.com/jprupp/rocksdb-haskell/blob/master/CHANGELOG.md)
- [Report a bug](https://github.com/jprupp/rocksdb-haskell/issues)

## Contents

- [Features](#features)
- [Installation](#installation)
- [Quick start](#quick-start)
- [Concepts](#concepts)
- [Usage guide](#usage-guide)
- [Configuration](#configuration)
- [Module overview](#module-overview)
- [Things to watch out for](#things-to-watch-out-for)
- [Development](#development)
- [Related projects](#related-projects)
- [License](#license)

## Features

| Area           | What you get                                                                   |
| -------------- | ------------------------------------------------------------------------------ |
| Basic access   | `get`, `put`, `delete` on strict `ByteString` keys and values                  |
| Batches        | Atomic `write` of many puts and deletes, across column families                |
| Multi-get      | `multiGet` reads many keys in one call to RocksDB's `MultiGet`                 |
| Column families| Open a database with several independent key spaces; add new ones on open     |
| Snapshots      | Consistent point-in-time reads across many queries and iterators               |
| Iterators      | Ordered scans, forwards and backwards, optionally pinned to a snapshot         |
| Tuning         | Bloom filters, fixed-length prefix extractor, shared LRU block cache, memtable and WAL limits, open file limit |
| Administration | Properties (memory use, statistics, SST files), approximate sizes, destroy, repair |

### What is not included

- No transactions (`TransactionDB`, `OptimisticTransactionDB`), merge
  operators, TTL databases, backups or checkpoints.
- No manual compaction, `DeleteRange` or `SingleDelete`.
- No custom comparators: keys always sort by their bytes.

Pull requests that add bindings for these are welcome.

## Installation

### System library

You need the RocksDB C library and its headers.

```sh
# Debian and Ubuntu
sudo apt-get install librocksdb-dev

# Fedora
sudo dnf install rocksdb-devel

# macOS with Homebrew
brew install rocksdb

# Nix
nix-shell -p rocksdb
```

The library links against `-lrocksdb`. If RocksDB is installed in a
non-standard place, pass its directories with `--extra-include-dirs` and
`--extra-lib-dirs` (Cabal or Stack).

### With Cabal

```cabal
build-depends:
    base
  , bytestring
  , data-default
  , rocksdb-haskell-jprupp
```

`data-default` gives you `def` for the default `Config`.

### With Stack

The package is in Stackage nightly snapshots. If your resolver does not have
it, or has an older version, add it to `stack.yaml`:

```yaml
extra-deps:
  - rocksdb-haskell-jprupp-2.6.0
```

With `stack --nix`, add RocksDB to the Nix packages:

```yaml
nix:
  packages:
    - rocksdb
```

## Quick start

```haskell
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Default (def)
import Database.RocksDB

main :: IO ()
main = do
  let cfg = def {createIfMissing = True, bloomFilter = True}
  withDB "/tmp/example" cfg $ \db -> do
    put db "hello" "world"
    get db "hello" >>= print -- Just "world"

    -- atomic batch
    write db [Put "a" "1", Put "b" "2", Del "hello"]

    -- many keys at once
    multiGet db ["a", "b", "c"] >>= print -- [Just "1",Just "2",Nothing]

    -- every entry, in key order
    withIter db $ \it -> do
      iterFirst it
      let loop =
            iterEntry it >>= \case
              Nothing -> return ()
              Just kv -> print kv >> iterNext it >> loop
      loop
```

## Concepts

### The database handle

`withDB` and `withDBCF` open a database, pass a `DB` to your function, and close
it when the function returns or throws. The `DB` must not escape that function.
It is safe to use from many threads at the same time.

### Keys and values

Keys and values are strict `ByteString`s. RocksDB sorts keys by their bytes,
which is the order iterators walk them in. To store Haskell types, serialize
them yourself, or use [rocksdb-query](https://github.com/jprupp/rocksdb-query).

### Column families

A database can hold several key spaces called column families, each with its
own configuration. Functions ending in `CF` act on a given column family; the
others act on the default one. The handles are in `columnFamilies db`, in the
order you listed them when opening.

### Snapshots

`withSnapshot db f` passes `f` a `DB` that reads from a frozen view of the
database. Reads and iterators through it do not see later writes. Writes
through it still go to the live database.

### Errors

Failures reported by RocksDB are thrown as `IOError` (built with `userError`).
The message starts with the operation, such as `put: ...` or `open: ...`.

## Usage guide

### Column families

```haskell
withDBCF "/tmp/example-cf" cfg [("users", cfg), ("orders", cfg)] $ \db -> do
  let [users, orders] = columnFamilies db
  putCF db users "alice" "..."
  getCF db orders "1234" >>= print
  write db [PutCF users "bob" "...", DelCF orders "1234"]
```

On an empty directory, `withDBCF` creates the database and all the column
families. On an existing database, RocksDB needs every column family that
exists to be listed. To add a new one, list it and set
`createMissingColumnFamilies = True` in the database `Config`.

### Snapshots

```haskell
withSnapshot db $ \snap -> do
  a <- get snap "a"
  b <- get snap "b" -- same point in time as "a"
  withIter snap $ \it -> iterFirst it >> iterEntry it >>= print
```

When the snapshot's lifetime does not fit a scope, use `createSnapshot` and
`releaseSnapshot`. `withIterSnap` and `withIterSnapCF` iterate over a raw
`Snapshot`.

### Iterators

An iterator starts out unpositioned. Move it with `iterFirst`, `iterLast` or
`iterSeek`, then step with `iterNext` and `iterPrev`. `iterKey`, `iterValue` and
`iterEntry` return `Nothing` once it moves past either end.

```haskell
-- every key that starts with "user:"
withIter db $ \it -> do
  iterSeek it "user:"
  let go acc =
        iterEntry it >>= \case
          Just (k, v) | "user:" `B.isPrefixOf` k -> iterNext it >> go ((k, v) : acc)
          _ -> return (reverse acc)
  users <- go []
  iterGetError it >>= mapM_ (fail . show) -- tell the end apart from an error
  return users
```

`createIterator`, `createIteratorSnap` and `destroyIterator` manage iterators
by hand.

### Properties and maintenance

```haskell
getProperty db Stats >>= mapM_ BS.putStr
getProperty db BlockCacheUsage
getPropertyCF db cf CurSizeAllMemTables
approximateSize db ("a", "z")
```

`destroy` deletes a closed database and `repair` tries to recover a damaged one.
Both take a raw `Options` pointer, which the public API does not let you build
yet, so in practice they are only usable from inside the library.

## Configuration

`Config` has a `Default` instance: all flags off and all sizes left to RocksDB.

| Field                         | Scope           | Meaning                                                         |
| ----------------------------- | --------------- | --------------------------------------------------------------- |
| `createIfMissing`             | database        | Create the database if it does not exist                        |
| `errorIfExists`               | database        | Fail if the database already exists                             |
| `paranoidChecks`              | database        | Check data aggressively and stop on corruption                  |
| `createMissingColumnFamilies` | database        | Create column families given to `withDBCF` that are missing     |
| `maxFiles`                    | database        | Maximum number of open files (default unlimited)                |
| `dbWriteBufferSize`           | database        | Bytes of all memtables together (default unlimited)             |
| `blockCacheSize`              | database        | Bytes of an LRU block cache shared by all column families       |
| `maxTotalWalSize`             | database        | Bytes of write-ahead log that trigger flushes                   |
| `writeBufferSize`             | column family   | Bytes per memtable (RocksDB default 64 MiB)                     |
| `bloomFilter`                 | column family   | Full Bloom filter with 10 bits per key                          |
| `prefixLength`                | column family   | Length of a fixed key prefix for the prefix extractor           |
| `cacheIndexAndFilterBlocks`   | column family   | Keep index and filter blocks in the block cache                 |

Database-wide fields are ignored in the configuration of a column family.

For a large database, setting `maxFiles`, `blockCacheSize`,
`cacheIndexAndFilterBlocks` and `dbWriteBufferSize` puts a ceiling on the memory
RocksDB uses outside the Haskell heap. Without them, index and filter blocks of
every open file and every memtable can grow without bound.

## Module overview

`Database.RocksDB` is the only exposed module. It re-exports:

| Part                     | Contents                                                       |
| ------------------------ | -------------------------------------------------------------- |
| Types                    | `DB`, `Config`, `BatchOp`, `ColumnFamily`, `Snapshot`, `Iterator`, `Property`, `Range` |
| Opening                  | `withDB`, `withDBCF`                                           |
| Reading and writing      | `get`, `getCF`, `multiGet`, `multiGetCF`, `put`, `putCF`, `delete`, `deleteCF`, `write` |
| Snapshots                | `withSnapshot`, `createSnapshot`, `releaseSnapshot`            |
| Iteration                | `withIter`, `withIterCF`, `withIterSnap`, `withIterSnapCF`, `iterSeek`, `iterFirst`, `iterLast`, `iterNext`, `iterPrev`, `iterKey`, `iterValue`, `iterEntry`, `iterValid`, `iterGetError` |
| Administration           | `getProperty`, `getPropertyCF`, `approximateSize`, `destroy`, `repair` |

The FFI bindings (`Database.RocksDB.C`) and helpers (`Database.RocksDB.Internal`)
are hidden modules.

## Things to watch out for

- Do not use a `DB`, snapshot or iterator after the function that gave it to you
  has returned. The C object is gone and RocksDB will crash.
- Only one process can open a database at a time. A second open fails with a
  lock error.
- Moving an invalid iterator does nothing. Reposition it with `iterSeek`,
  `iterFirst` or `iterLast`.
- An iterator that becomes invalid because of corruption looks like one that
  reached the end. Check `iterGetError`.
- Calls into RocksDB are `safe` foreign calls, so they do not block other
  Haskell threads, but each one costs a little more than an `unsafe` call.

## Development

### Stack

```sh
git clone https://github.com/jprupp/rocksdb-haskell.git
cd rocksdb-haskell

stack build                        # build the library
stack test                         # run the tests
stack haddock --no-haddock-deps    # build the API documentation
```

With `stack --nix`, the `nix` section of `stack.yaml` provides RocksDB.

### Nix

```sh
nix develop
cabal build all
cabal test all
```

The shell includes GHC 9.8.4, cabal, haskell-language-server, fourmolu, hlint
and hoogle.

`package.yaml` is the source of the package description. Change it rather than
`rocksdb-haskell-jprupp.cabal`, which [hpack](https://github.com/sol/hpack)
generates.

The tests are in `test/Spec.hs` and use [hspec](https://hspec.github.io/) and
QuickCheck. Each test runs against a fresh database in a temporary directory.

### Contributing

Pull requests are welcome.

- Format new code with fourmolu, which the Nix shell provides.
- Add tests for the behaviour you change.
- Describe the change in `CHANGELOG.md`. Versions follow the
  [Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Related projects

- [rocksdb-query](https://github.com/jprupp/rocksdb-query) stores and queries
  Haskell types with these bindings.
- [haskoin-store](https://github.com/jprupp/haskoin-store) is a Bitcoin block
  chain indexer built on them.

This library started as a fork of
[leveldb-haskell](https://github.com/kim/leveldb-haskell) and of earlier RocksDB
bindings. See [AUTHORS](https://github.com/jprupp/rocksdb-haskell/blob/master/AUTHORS).

## License

rocksdb-haskell-jprupp is released under the
[BSD 3-Clause License](https://github.com/jprupp/rocksdb-haskell/blob/master/LICENSE).
