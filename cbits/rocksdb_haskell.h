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
/* of rocksdb_haskell_iter_chunk and rocksdb_haskell_options_set_merge_operator */
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

/*
 * The two functions of a merge operator, with the types that
 * rocksdb_mergeoperator_create of rocksdb/c.h asks for.
 *
 * The first argument of both is the state of the operator, which belongs
 * to rocksdb_haskell_options_set_merge_operator: a function must not use
 * it. "key" is the key that is merged, for an operator that needs it.
 *
 * A full merge works out the value of a key: from the value that the key
 * has ("existing_value", which is NULL when it has none) and the
 * "num_operands" merge records written since, oldest first, at
 * "operands_list" with their lengths at "operands_list_length".
 *
 * A partial merge is given merge records only, oldest first, and makes one
 * record of them that does to any value what they did one after another.
 * RocksDB uses it to shorten the list before the value is known, when it
 * compacts files that do not have it.
 *
 * Both return the new bytes in memory from malloc(), which RocksDB frees
 * with free(), their length in "*new_value_length", and 1 in "*success".
 * To fail, a function stores 0 in "*success" and in "*new_value_length",
 * and returns NULL: RocksDB copies that many bytes from the result before
 * it looks at "*success". A read of the key then fails, and so does a
 * compaction that meets it. A partial merge that fails is not an error:
 * RocksDB keeps the records as they are.
 *
 * They are called from the threads of RocksDB, during reads, flushes and
 * compactions, several at a time. They must be plain C: a function that
 * enters the Haskell run-time system from such a thread is not what this
 * is for.
 */
typedef char *(*rocksdb_haskell_full_merge_t)(
    void *state,
    const char *key,
    size_t key_length,
    const char *existing_value,
    size_t existing_value_length,
    const char *const *operands_list,
    const size_t *operands_list_length,
    int num_operands,
    unsigned char *success,
    size_t *new_value_length);

typedef char *(*rocksdb_haskell_partial_merge_t)(
    void *state,
    const char *key,
    size_t key_length,
    const char *const *operands_list,
    const size_t *operands_list_length,
    int num_operands,
    unsigned char *success,
    size_t *new_value_length);

/*
 * Give the options of a database or of a column family a merge operator
 * made of a name and two functions.
 *
 * Why: rocksdb_mergeoperator_create wants a state with a destructor and a
 * function that returns the name, besides the two that merge. A Haskell
 * program has no C function to give for those, and should not make one
 * that calls back into Haskell, since RocksDB calls them from its own
 * threads. This function makes the operator around a copy of the name,
 * which is all the state it has and is freed with the operator.
 *
 * The options own the operator from then on, and the column families
 * opened with them keep it for as long as they need it: there is nothing
 * for the caller to destroy. RocksDB frees what the two functions return
 * with free(), since the operator is made without a function of its own
 * for that.
 *
 * The result is ROCKSDB_HASKELL_OK, or ROCKSDB_HASKELL_NO_MEMORY with the
 * options as they were.
 */
int rocksdb_haskell_options_set_merge_operator(
    rocksdb_options_t *options,
    const char *name,
    rocksdb_haskell_full_merge_t full_merge,
    rocksdb_haskell_partial_merge_t partial_merge);

#endif
