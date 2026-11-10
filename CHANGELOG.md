## 2.6.4

 * Iterators read rows about three times faster. `iterValid`, `iterKey` and
   `iterValue` now reach RocksDB through unsafe foreign calls. The three only
   read what the iterator already holds, and reading one row with `iterEntry`
   and `iterNext` makes five of them; as safe calls each cost about 0.4
   microseconds. Walking 700,000 rows took 2.8 microseconds a row before and
   0.8 after. The calls that move an iterator, which can read from disk, are
   safe calls as before.

## 2.6.3

 * Documentation only; the library behaves as in 2.6.2.
 * References in the Haddock that named both a type and its constructor, such
   as `DB` and `Config`, now say that they mean the type, and two that named
   nothing in scope are fixed: the documentation builds without warnings about
   ambiguous or unresolved identifiers.
 * The documentation of `withIter` said to set a snapshot in the read options,
   which the API has no way to do. It now says how: create the iterator from
   the `DB` that `withSnapshot` provides.
 * Remove an unused import.
 * The test suite grows from 17 tests to 51. It now covers deletes, batches,
   `withSnapshot`, iteration backwards, empty keys and values, binary data,
   and the errors of opening a database, none of which had a test. Six of the
   tests are QuickCheck properties that compare the database with a `Map`
   after random writes. The suite also compiles without warnings.
 * A badge in the README shows the state of CI on `master`.

## 2.6.2

 * Build the Bloom filters that `bloomFilter` asks for. The bits per key were
   passed to `rocksdb_filterpolicy_create_bloom_full` as an integer, which is
   what RocksDB took until 6.23; since 6.24 the function takes a `double`. An
   integer and a `double` travel in different registers, so RocksDB read
   something else as the bits per key and built no filters: table files had
   `filter block size=0`, and a lookup of an absent key read a data block in
   every file that could hold it. Nothing failed, and no test noticed. In a
   measurement with 2,600,000 keys, such a lookup took about 5.5 microseconds
   without the filters and 1.5 with them.

   The function is now imported with the `capi` calling convention, through a
   stub compiled against `rocksdb/c.h`, so the C compiler passes the argument
   as the installed RocksDB declares it, before or after 6.24. The headers
   were already needed to install RocksDB for development; they are now
   needed to build this package too.

   Databases opened with `bloomFilter` start building filters when they are
   opened with this version: for the table files written from then on, at
   about 1.25 bytes per key. Files written before have none until compaction
   rewrites them.
 * Pass the error pointer that `rocksdb_approximate_sizes` takes since RocksDB
   6.16. `approximateSize` called it with one argument too few, leaving RocksDB
   to read a pointer that was never passed, and to write an error message
   through it if the call failed. It now throws an `IOError` on failure.
 * Both were found by comparing every foreign import with the `rocksdb/c.h` of
   RocksDB 11.1: the other 70 match.
 * Tests that table files have filters when asked for and not otherwise, and
   of `approximateSize`, which had none.

## 2.6.1

 * Documentation, lots of it, with detailed explanations.

## 2.6.0

 * Add `multiGet` and `multiGetCF`, which read the values of many keys in
   one call to RocksDB's `MultiGet`.
 * Add `blockCacheSize` to `Config`: the bytes of a least-recently-used block
   cache shared by the database and all its column families. Without it
   RocksDB gives each column family its own default cache, of 32 MiB in
   RocksDB 8.
 * Add `cacheIndexAndFilterBlocks` to `Config`, to keep the index and filter
   blocks of a column family in the block cache, at high priority, with
   those of level 0 pinned, instead of in memory outside the cache for every
   open file.
 * Stop leaking the block-based table options of every database and column
   family opened with a Bloom filter.

## 2.5.0

 * Add the `EstimateTableReadersMem`, `CurSizeAllMemTables` and
   `BlockCacheUsage` properties, and `PropertyName` for any other property by
   its name.
 * Add `getPropertyCF` to read a property of a column family.
 * Add `maxTotalWalSize` (`max_total_wal_size`) to `Config`, to bound the
   write-ahead log.
 * Add `createMissingColumnFamilies` (`create_missing_column_families`) to
   `Config`, to add column families to an existing database.
 * Destroy the handle of the default column family when `withDBCF` opens an
   existing database. It was leaked, and RocksDB built with assertions
   aborted when the database was closed.

## 2.4.0

 * Add `writeBufferSize` (`write_buffer_size`) and `dbWriteBufferSize`
   (`db_write_buffer_size`) to `Config`, to bound memtable memory.

## 2.3.1

 * Do not call next/prev on invalid iterators (fixes assertion failure).

## 2.3.0

 * Remove UnliftIO and remain in IO monad.

## 2.2.0

 * Move Iterator module to plain IO.
 * Properly manage read options in createSnapshot.
 * Add nix flake for development.
 * API allows manual database lifecycle management.

## 2.1.7

 * Simplify author name.

## 2.1.6

 * Refactor memory use further.
 * Improve tests.

## 2.1.5

 * Fix errors not releasing memory.

## 2.1.4

 * Add ResourceT support for snapshots and iterators.

## 2.1.3

 * Correct bug where database was ignoring prefix length config.


## 2.1.2

 * Correct bug when opening database with column families.


## 2.1.1

 * Expose ColumnFamily type.
 

## 2.1.0

 * Add support for column families.


## 2.0.0

 * Fork into a different project
 * Rewrite most of the code to make it faster and easier to use
 * Remove many features that I don't need or want to maintain


## 1.0.1

 * Add support for UTF-8 characters in a database's path


## 1.0.0

 * First version by Serokell


## 0.3x.0

 * ResourceT is no longer compulsory


## 0.2.0

 * requires LevelDB v1.7
 * support for filter policy (LevelDB v1.5), either custom or using the built-in
   bloom filter implementation
 * write batch values no longer require a `memcpy` to be early-finalizer-safe
   (introduced in 0.1.1)


## 0.1.0

 * memory (foreign pointers) is managed through
   [ResourceT](http://hackage.haskell.org/package/resourcet). Note that this
   requires to lift monadic actions inside the `MonadResource` monad, see the
   examples.
 * links against shared library (LevelDB v1.3 or higher)
 * LevelDB 1.3 API fully supported (including custom comparators, excluding
   custom environments)


## 0.0.x

 * experimental releases
