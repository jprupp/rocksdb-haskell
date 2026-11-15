/*
 * C helpers of rocksdb-haskell-jprupp: what the binding needs and the C API
 * of RocksDB does not have.
 *
 * The Haskell imports in Database.RocksDB.C name this header with the capi
 * calling convention, so the C compiler checks each of them against the
 * prototypes below.
 */

#ifndef ROCKSDB_HASKELL_H
#define ROCKSDB_HASKELL_H

#include <stddef.h>
#include <stdint.h>

#include <rocksdb/c.h>

/* Results of the functions below. */
#define ROCKSDB_HASKELL_OK 0
/* of rocksdb_haskell_iter_chunk */
#define ROCKSDB_HASKELL_NO_MEMORY 1
/* of rocksdb_haskell_writebatch_add_packed */
#define ROCKSDB_HASKELL_MALFORMED 2

/*
 * Read entries of an iterator, from the one it is on, in one call.
 *
 * Why: a Haskell program pays for every foreign call that may block, so a
 * scan that calls rocksdb_iter_next once for each entry pays once for each
 * entry. This function walks the iterator in C and copies the entries out,
 * so that the scan pays once for a few hundred.
 *
 * It goes forwards with rocksdb_iter_next, or backwards with
 * rocksdb_iter_prev if "backwards" is not zero, and stops at the first of:
 *
 *  - an iterator that is not valid: the end of the data or of its bounds, or
 *    an error;
 *  - an entry whose key does not start with the "prefix_len" bytes at
 *    "prefix" (no bytes: every key does);
 *  - "max_entries" entries read;
 *  - an entry whose key and value would take the bytes of keys and values
 *    read to more than "max_bytes". The first entry is read whatever its
 *    size, so that a scan always moves.
 *
 * The iterator is left on the first entry that was not read. "*more" tells
 * whether that is an entry whose key starts with the prefix: whether there is
 * anything left for another call.
 *
 * "*count" is the number of entries read, and "*buffer" holds them, one
 * after another, each laid out as
 *
 *     size_t key_len; size_t value_len; key bytes; value bytes; padding
 *
 * with padding up to a multiple of sizeof(size_t), so that the lengths are
 * always aligned. The caller frees "*buffer" with free(); it is NULL when no
 * entry was read.
 *
 * Errors:
 *
 *  - An error of the iterator is stored in "*errptr" as by
 *    rocksdb_iter_get_error: a message that the caller frees. Nothing is
 *    returned then: "*buffer" is NULL and "*count" is zero. "*errptr" must be
 *    NULL on entry.
 *  - If memory runs out with some entries read, they are returned as a short
 *    chunk, and the iterator is on the entry that did not fit. If it runs out
 *    on the first entry, the result is ROCKSDB_HASKELL_NO_MEMORY, with
 *    nothing returned and the iterator where it was.
 *
 * The result is ROCKSDB_HASKELL_OK otherwise, also when "*errptr" is set.
 */
int rocksdb_haskell_iter_chunk(
    rocksdb_iterator_t *iter,
    int backwards,
    size_t max_entries,
    size_t max_bytes,
    const char *prefix,
    size_t prefix_len,
    char **buffer,
    size_t *count,
    unsigned char *more,
    char **errptr);

/* The operation of a record of rocksdb_haskell_writebatch_add_packed. */
#define ROCKSDB_HASKELL_PUT 0
#define ROCKSDB_HASKELL_DELETE 1
#define ROCKSDB_HASKELL_MERGE 2

/*
 * The bytes that come before the key of such a record: a column family
 * handle, two lengths and an operation.
 */
#define ROCKSDB_HASKELL_RECORD_HEADER \
    (sizeof(rocksdb_column_family_handle_t *) + 2 * sizeof(uint32_t) + 1)

/*
 * Add many operations to a write batch in one call.
 *
 * Why: a Haskell program pays for every foreign call that may block, so a
 * batch that is built with rocksdb_writebatch_put_cf pays once for each
 * key. For the 65,000 keys of a Bitcoin block that was more than half of
 * the time that the batch took to write, and nearly all of it when other
 * threads of the program were busy: bench/Batch.hs. This function takes
 * the operations packed one after another in a buffer and makes those
 * calls from C, so that the program pays once for a few thousand.
 *
 * "records" holds "len" bytes: any number of records, with nothing between
 * them and nothing after the last. A record is
 *
 *     rocksdb_column_family_handle_t *family;   NULL: the default one
 *     uint32_t key_len;
 *     uint32_t value_len;                       zero for a delete
 *     unsigned char operation;                  ROCKSDB_HASKELL_PUT, ...
 *     key bytes; value bytes
 *
 * with no padding anywhere: the header is ROCKSDB_HASKELL_RECORD_HEADER
 * bytes, 17 where a pointer has eight, and nothing in it is aligned. The
 * handle and the lengths are as this machine holds them in memory, written
 * with memcpy or its like. Being unaligned is what lets a program write a
 * record where the last one ended, in a buffer of its own, and fill in the
 * lengths once it has serialised the key and the value after the header.
 *
 * The operations are added in the order of the records, each as the
 * function of RocksDB that it names would add it: rocksdb_writebatch_put,
 * rocksdb_writebatch_delete or rocksdb_writebatch_merge for the default
 * column family, and the ones ending in _cf for a handle. RocksDB copies
 * the keys and values, so the buffer is not needed after the call.
 *
 * A merge is only added here. Whether the column family has a merge
 * operator to apply it with is for RocksDB to say, when the batch is
 * written or when the key is read.
 *
 * The buffer is checked before anything is added, and the call never reads
 * outside it. If a record is cut short by the end of the buffer, names an
 * operation that does not exist, or is a delete with a value, the result is
 * ROCKSDB_HASKELL_MALFORMED, the batch is as it was, and "*count" is the
 * number of whole records before the bad one. A handle cannot be checked:
 * one that is not NULL has to be a column family of the database that the
 * batch is written to.
 *
 * The result is ROCKSDB_HASKELL_OK otherwise, and "*count" is the number of
 * operations added.
 */
int rocksdb_haskell_writebatch_add_packed(
    rocksdb_writebatch_t *batch,
    const char *records,
    size_t len,
    size_t *count);

#endif
