## 2.8.0

 * `maxBackgroundJobs`, a new field of `Config`: how many threads RocksDB may
   use at once to flush memtables and to compact files, where its default is
   two, one of each. A database that has a backlog of compactions, after a
   bulk load for instance, works through it with one thread unless it is
   given more. More threads only help where the disk and the cores have room
   for them: a compaction that already waits for a slow disk is not made
   faster by a second one, and both take from the reads.
 * `partitionIndexAndFilters`, another new field of `Config`: the index and
   the filter of each table file are written in pieces of 4 KiB under a small
   index of their own. Each is otherwise one block that grows with the file,
   megabytes for a file of 64 MiB with a filter, and a database that has more
   of them than its block cache holds loads and drops them at nearly every
   lookup. Measured on a database of 1.6 TB with a cache of 1 GiB: the cache
   held the index and filter of 400 of 32,000 files and three megabytes of
   data, and a lookup took 0.8 ms of processor time on an idle disk. With
   pieces, a lookup loads the two that it needs. It applies to files written
   from then on; existing files keep their layout until compacted.
 * `compactRange` writes again every table file of a range of keys, those of
   the last level included, so that the files a database already has get the
   layout of its present `Config`: a Bloom filter, or
   `partitionIndexAndFilters`. RocksDB's own compactions rarely reach the
   last level, where most of the data is.
 * Many entries of an iterator in one foreign call. `iterNext` and `iterPrev`
   can wait for the disk, so each is a call for which the Haskell thread
   gives up its capability and has to get one back, and a scan made one for
   every entry. `iterChunk` walks the iterator in C, in a small function
   that now comes with the package (`cbits/rocksdb_haskell.c`), and returns
   up to 256 entries or 1 MiB from one call, forwards or backwards, within a
   prefix if one is given, with a flag that tells whether more follow. A
   `ChunkConfig` sets those limits. `iterEntries`, `iterEntriesBack`,
   `iterEntriesPrefix` and `iterEntriesPrefixBack` are short forms for a
   known number of entries. All of them leave the iterator on the first
   entry that they did not return, respect the bounds of an `IterConfig`,
   and throw if the iterator stopped at an error. The single steps are
   unchanged.

   Seconds for every thread to read 200,000 entries of 52 bytes, on four
   capabilities and four cores (`stack bench`):

   | Scan                                  | 1 thread | 16 threads |
   | ------------------------------------- | -------- | ---------- |
   | Single steps, loop not tail recursive | 0.78     | 3.9        |
   | Single steps, loop tail recursive     | 0.090    | 0.50       |
   | Chunks of 256 entries                 | 0.048    | 0.25       |

   With all 24 hardware threads of the machine free, the sixteen threads
   took 4.4, 0.83 and 0.12 seconds: threads inside the call hold no
   capability, so chunks gain more where there are more cores than
   capabilities. The first row is a loop that returns its list with
   `(entry :) <$> go`; the run-time system walks the stack at every such
   call, and that loop leaves a frame on it for every entry.
 * A large batch in a few foreign calls. `write` hands RocksDB the
   operations of a batch one call at a time, each a call for which the
   Haskell thread gives up its capability and has to get one back. For the
   65,000 keys of a Bitcoin block that was more than half of the time of
   the batch, and nearly all of it in a program whose other threads were
   busy. `writePacked` takes the batch as chunks of packed records, and a
   second function of `cbits/rocksdb_haskell.c` adds the operations of a
   chunk to the batch in one call. The batch is then written as before:
   atomically, with one write, however many chunks it came in.

   `packOps` packs a list of `BatchOp` into a chunk. A program that
   serialises its keys and values itself can write records straight into
   its own buffers instead, with no `ByteString` for each: it leaves
   `packedHeaderSize` bytes, serialises the key and the value after them,
   and fills in the header with `pokePackedHeader` once the lengths are
   known. Records are not aligned, so the next one starts where the last
   one ended.

   A chunk is checked in C before anything of it is used. One that is cut
   short, that has a length reaching past its end, an operation that does
   not exist or a delete with a value, makes `writePacked` throw, with
   nothing written.

   A record can also ask for a merge (`PackedMerge`), which is handed to
   `rocksdb_writebatch_merge`. The binding still has no way to give a
   column family a merge operator, so RocksDB refuses such a batch; the
   format has it so that it need not change later.

   The format is this package's own. It is not RocksDB's internal encoding
   of a batch (`rocksdb_writebatch_create_from`), which would save a copy
   and tie the binding to the insides of RocksDB.

   Milliseconds to write a batch of 65,000 operations and 6 MB over four
   column families, on four capabilities and four cores
   (`stack bench rocksdb-haskell-jprupp:bench:batch`):

   | Way to write the batch                    | Alone | Next to 8 busy threads |
   | ----------------------------------------- | ----- | ---------------------- |
   | `write`: a call for each operation        | 48    | 420                    |
   | `writePacked`, with `packOps` in the time | 28    | 115                    |
   | `writePacked`, chunks packed beforehand   | 22    | 28                     |

   All three end with the same write, of about 20 ms. Alone, the calls of
   `write` cost 400 ns an operation. Next to threads that compute, the
   thread waits for a capability after each of its 65,000 calls. With all
   24 hardware threads of the machine free, that took `write` 1,160 ms,
   and packed chunks 31.
 * The `Config` record has new fields, so code that builds one without `def`
   has to set them; code that starts from `def` needs no change. This is why
   the version is 2.8.0 and not 2.7.2.

## 2.7.1

 * Documentation and tests only; the library behaves as in 2.7.0.
 * The documentation of `prefixSameAsStart` was wrong on one point and silent
   on another. It said that the option does nothing after `iterFirst` or
   `iterLast`: it does, the iterator stays within the prefix of the key that
   it finds there. And it did not say that every key, sought or stored, has
   to be at least as long as the `prefixLength`: RocksDB takes the prefix of
   a key without checking its length, and reads past the end of a shorter
   one. Both are now in the Haddock and in the README, and the first has
   tests.

## 2.7.0

 * Bounded iterators. `withIterConfig` creates an iterator that RocksDB itself
   stops, as an `IterConfig` says: at a `lowerBound` or an `upperBound`, or at
   the end of the prefix that it was sent to, with `prefixSameAsStart`. Until
   now a scan of a range could only stop by reading the first key past it,
   and when the keys past it had been deleted, RocksDB stepped over the marker
   of every one to find that key. A scan of an empty range followed by 20,000
   deleted keys took 2,600 microseconds; with an upper bound it takes 2.5.
   `prefixUpperBound` gives the bound that ends a prefix, and
   `createIteratorConfig` manages such an iterator by hand.
 * `totalOrderSeek`, also in `IterConfig`, walks every key in order in a
   database that has a `prefixLength`, where a plain iterator only promises
   the keys of one prefix.
 * The `DB` record has a new field, `snapshot`, with the snapshot that its
   read options read from. Code that builds a `DB` by hand has to set it;
   code that gets one from `withDB`, `withDBCF` or `withSnapshot` needs no
   change. This is why the version is 2.7.0 and not 2.6.5.

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
