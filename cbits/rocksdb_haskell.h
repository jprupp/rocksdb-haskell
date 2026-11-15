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

#include <rocksdb/c.h>

/* Results of rocksdb_haskell_iter_chunk. */
#define ROCKSDB_HASKELL_OK 0
#define ROCKSDB_HASKELL_NO_MEMORY 1

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

#endif
