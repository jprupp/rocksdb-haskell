# rocksdb-haskell-jprupp

Haskell bindings to [RocksDB](https://rocksdb.org), the embedded persistent
key-value store from Meta. The package on Hackage is called
`rocksdb-haskell-jprupp`; the module to import is `Database.RocksDB`.

The bindings follow the RocksDB C API closely, with a thin layer of
bracket-style resource management on top so that databases, snapshots and
iterators are always released.

## Features

- Open, read, write and delete with strict `ByteString` keys and values.
- Atomic batch writes.
- Column families.
- Multi-get: look up many keys in one call.
- Snapshots for consistent reads across many queries.
- Iterators, optionally pinned to a snapshot.
- Bloom filters, fixed-length key prefixes, and a shared LRU block cache.
- Database properties, approximate sizes, destroy and repair.

## Requirements

You need the RocksDB C library and its headers.

| System        | Command                                   |
|---------------|-------------------------------------------|
| Debian/Ubuntu | `sudo apt install librocksdb-dev`         |
| Fedora        | `sudo dnf install rocksdb-devel`          |
| macOS         | `brew install rocksdb`                    |

The Nix flake in this repository provides it as well.

## Usage

Add `rocksdb-haskell-jprupp` and `data-default` to your dependencies.

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Data.Default (def)
import Database.RocksDB

main :: IO ()
main = do
    let cfg = def { createIfMissing = True, bloomFilter = True }
    withDB "/tmp/example" cfg $ \db -> do
        put db "hello" "world"
        get db "hello" >>= print                 -- Just "world"

        -- atomic batch
        write db [Put "a" "1", Put "b" "2", Del "hello"]

        -- many keys at once
        multiGet db ["a", "b", "c"] >>= print    -- [Just "1",Just "2",Nothing]

        -- iterate in key order
        withIter db $ \it -> do
            iterFirst it
            let loop = iterEntry it >>= \case
                    Nothing -> return ()
                    Just kv -> print kv >> iterNext it >> loop
            loop
```

### Column families

```haskell
withDBCF "/tmp/example-cf" cfg [("users", cfg), ("orders", cfg)] $ \db -> do
    let [users, orders] = columnFamilies db
    putCF db users "alice" "..."
    getCF db orders "1234" >>= print
```

The handles in `columnFamilies` come in the same order as the list given to
`withDBCF`. To add a column family to an existing database, set
`createMissingColumnFamilies = True`.

### Snapshots

```haskell
withSnapshot db $ \snap -> do
    a <- get snap "a"
    b <- get snap "b"          -- same point in time as "a"
    withIter snap $ \it -> ...
```

`withIterSnap` and `withIterSnapCF` take a raw `Snapshot` instead, for use
with `createSnapshot`.

### Configuration

`Config` has a `Default` instance. Notable fields:

| Field                         | Meaning                                                      |
|-------------------------------|--------------------------------------------------------------|
| `createIfMissing`             | Create the database if it does not exist.                    |
| `errorIfExists`               | Fail if the database already exists.                         |
| `createMissingColumnFamilies` | Create column families given to `withDBCF` that are missing. |
| `bloomFilter`                 | Use a 10-bit-per-key bloom filter.                           |
| `prefixLength`                | Fixed key prefix length for the prefix extractor.            |
| `maxFiles`                    | Maximum number of open files.                                |
| `writeBufferSize`             | Bytes per memtable.                                          |
| `dbWriteBufferSize`           | Bytes for all memtables together.                            |
| `blockCacheSize`              | Size of an LRU block cache shared by all column families.    |
| `cacheIndexAndFilterBlocks`   | Keep index and filter blocks in the block cache.             |
| `maxTotalWalSize`             | Write-ahead log size that triggers flushes.                  |

Fields marked as database-wide in the Haddock documentation are ignored in
the configuration of a column family.

### Errors

Failures reported by RocksDB are thrown as `IOError` (`userError`), with a
message naming the operation, such as `put: ...`. After iterating, call
`iterGetError` to tell the end of the data apart from corruption.

## Development

### Stack

```bash
stack build
stack test
stack haddock
```

### Nix

```bash
nix develop
cabal build all
cabal test all
```

The shell includes GHC 9.8.4, cabal, haskell-language-server, fourmolu,
hlint, and hoogle.

## History

This library started as a fork of
[leveldb-haskell](https://github.com/kim/leveldb-haskell) and of earlier
RocksDB bindings. See [AUTHORS](AUTHORS) and [CHANGELOG.md](CHANGELOG.md).

## License

BSD 3-Clause. See [LICENSE](LICENSE).
