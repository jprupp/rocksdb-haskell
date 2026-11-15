/*
 * C helpers of rocksdb-haskell-jprupp. See rocksdb_haskell.h for what each
 * function is for and what it promises.
 */

#include "rocksdb_haskell.h"

#include <stdint.h>
#include <stdlib.h>
#include <string.h>

/* The two lengths that come before the bytes of an entry. */
#define HEADER (2 * sizeof(size_t))

/* The first size of the buffer, which doubles when it is full. */
#define FIRST_CAPACITY 1024

/*
 * Whether the iterator is on an entry whose key starts with the prefix. If
 * it is, the key is in "*key" and "*key_len", and belongs to the iterator
 * until it moves.
 */
static int at_match(
    const rocksdb_iterator_t *iter,
    const char *prefix,
    size_t prefix_len,
    const char **key,
    size_t *key_len)
{
    if (!rocksdb_iter_valid(iter)) {
        return 0;
    }
    *key_len = 0;
    *key = rocksdb_iter_key(iter, key_len);
    if (*key == NULL) {
        /* no bytes to read, whatever the length says */
        *key_len = 0;
    }
    if (*key_len < prefix_len) {
        return 0;
    }
    /* memcmp must not be given a null pointer, even to compare no bytes */
    return prefix_len == 0 || memcmp(*key, prefix, prefix_len) == 0;
}

/*
 * The size that an entry takes in the buffer: header, key, value and
 * padding. Zero if it does not fit in a size_t, which no real entry does.
 */
static size_t entry_size(size_t key_len, size_t value_len)
{
    const size_t align = sizeof(size_t);
    size_t size = HEADER;

    if (key_len > SIZE_MAX - size) {
        return 0;
    }
    size += key_len;
    if (value_len > SIZE_MAX - size) {
        return 0;
    }
    size += value_len;
    if (size > SIZE_MAX - (align - 1)) {
        return 0;
    }
    return (size + (align - 1)) / align * align;
}

/*
 * Make room for "size" more bytes after the "used" ones of the buffer.
 * Returns zero, with the buffer as it was, if there is no memory for it.
 */
static int reserve(char **buffer, size_t *capacity, size_t used, size_t size)
{
    size_t needed;
    size_t grown;
    char *moved;

    if (size > SIZE_MAX - used) {
        return 0;
    }
    needed = used + size;
    if (needed <= *capacity) {
        return 1;
    }
    grown = *capacity < FIRST_CAPACITY ? FIRST_CAPACITY : *capacity;
    while (grown < needed) {
        if (grown > SIZE_MAX / 2) {
            grown = needed;
        } else {
            grown *= 2;
        }
    }
    moved = realloc(*buffer, grown);
    if (moved == NULL) {
        return 0;
    }
    *buffer = moved;
    *capacity = grown;
    return 1;
}

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
    char **errptr)
{
    char *entries = NULL; /* the buffer */
    size_t capacity = 0;  /* its size */
    size_t used = 0;      /* how much of it holds entries */
    size_t read = 0;      /* how many entries */
    size_t bytes = 0;     /* their keys and values, without the rest */
    int no_memory = 0;

    *buffer = NULL;
    *count = 0;
    *more = 0;

    while (read < max_entries) {
        const char *key;
        const char *value;
        size_t key_len;
        size_t value_len = 0;
        size_t size;
        char *entry;

        if (!at_match(iter, prefix, prefix_len, &key, &key_len)) {
            break;
        }
        value = rocksdb_iter_value(iter, &value_len);
        if (value == NULL) {
            value_len = 0;
        }

        size = entry_size(key_len, value_len);
        if (size == 0) {
            no_memory = 1;
            break;
        }
        /*
         * The limit on bytes, which the first entry is exempt from. Since
         * entry_size did not overflow, neither does the sum of the lengths.
         * "bytes" is above "max_bytes" only after a first entry that was.
         * At the limit exactly, an entry of no bytes is still read: it
         * does not take the bytes read to more.
         */
        if (read > 0 && (bytes > max_bytes || key_len + value_len > max_bytes - bytes)) {
            break;
        }
        if (!reserve(&entries, &capacity, used, size)) {
            no_memory = 1;
            break;
        }

        entry = entries + used;
        memcpy(entry, &key_len, sizeof(size_t));
        memcpy(entry + sizeof(size_t), &value_len, sizeof(size_t));
        if (key_len > 0) {
            memcpy(entry + HEADER, key, key_len);
        }
        if (value_len > 0) {
            memcpy(entry + HEADER + key_len, value, value_len);
        }
        used += size;
        bytes += key_len + value_len;
        read += 1;

        /* only now is the entry safe, and the iterator may leave it */
        if (backwards) {
            rocksdb_iter_prev(iter);
        } else {
            rocksdb_iter_next(iter);
        }
    }

    if (no_memory) {
        /* the iterator is still on the entry that did not fit */
        if (read == 0) {
            free(entries);
            return ROCKSDB_HASKELL_NO_MEMORY;
        }
        *buffer = entries;
        *count = read;
        *more = 1;
        return ROCKSDB_HASKELL_OK;
    }

    /*
     * An iterator that met an error is not valid, like one that reached the
     * end. Ask it, so that the caller does not take one for the other.
     */
    rocksdb_iter_get_error(iter, errptr);
    if (*errptr != NULL) {
        free(entries);
        return ROCKSDB_HASKELL_OK;
    }

    {
        const char *key;
        size_t key_len;

        *more = at_match(iter, prefix, prefix_len, &key, &key_len) ? 1 : 0;
    }
    *buffer = entries;
    *count = read;
    return ROCKSDB_HASKELL_OK;
}

/* A record of a packed batch, read out of its buffer. */
struct record {
    rocksdb_column_family_handle_t *family; /* NULL: the default one */
    uint32_t key_len;
    uint32_t value_len;
    unsigned char operation;
    const char *key;   /* in the buffer */
    const char *value; /* in the buffer, after the key */
    size_t size;       /* of the whole record, header included */
};

/*
 * Read the record at the start of the "left" bytes at "bytes". Returns zero
 * if there is no whole and well formed record there, and reads nothing past
 * those bytes either way.
 *
 * The header is copied out with memcpy because it is not aligned: a record
 * starts wherever the one before it ended.
 */
static int record_at(const char *bytes, size_t left, struct record *record)
{
    const size_t header = ROCKSDB_HASKELL_RECORD_HEADER;
    const char *field = bytes;

    if (left < header) {
        return 0;
    }
    memcpy(&record->family, field, sizeof(record->family));
    field += sizeof(record->family);
    memcpy(&record->key_len, field, sizeof(uint32_t));
    field += sizeof(uint32_t);
    memcpy(&record->value_len, field, sizeof(uint32_t));
    field += sizeof(uint32_t);
    record->operation = (unsigned char)*field;

    if (record->operation > ROCKSDB_HASKELL_MERGE) {
        return 0;
    }
    if (record->operation == ROCKSDB_HASKELL_DELETE && record->value_len != 0) {
        return 0;
    }
    /* one length at a time, so that their sum cannot overflow */
    left -= header;
    if (record->key_len > left) {
        return 0;
    }
    left -= record->key_len;
    if (record->value_len > left) {
        return 0;
    }

    record->key = bytes + header;
    record->value = record->key + record->key_len;
    record->size = header + record->key_len + record->value_len;
    return 1;
}

/* Add a record to a batch, with the function of RocksDB that it names. */
static void add_record(rocksdb_writebatch_t *batch, const struct record *record)
{
    rocksdb_column_family_handle_t *family = record->family;
    const char *key = record->key;
    const char *value = record->value;
    size_t key_len = record->key_len;
    size_t value_len = record->value_len;

    switch (record->operation) {
    case ROCKSDB_HASKELL_PUT:
        if (family == NULL) {
            rocksdb_writebatch_put(batch, key, key_len, value, value_len);
        } else {
            rocksdb_writebatch_put_cf(batch, family, key, key_len, value, value_len);
        }
        break;
    case ROCKSDB_HASKELL_DELETE:
        if (family == NULL) {
            rocksdb_writebatch_delete(batch, key, key_len);
        } else {
            rocksdb_writebatch_delete_cf(batch, family, key, key_len);
        }
        break;
    case ROCKSDB_HASKELL_MERGE:
        if (family == NULL) {
            rocksdb_writebatch_merge(batch, key, key_len, value, value_len);
        } else {
            rocksdb_writebatch_merge_cf(batch, family, key, key_len, value, value_len);
        }
        break;
    }
}

int rocksdb_haskell_writebatch_add_packed(
    rocksdb_writebatch_t *batch,
    const char *records,
    size_t len,
    size_t *count)
{
    struct record record;
    size_t at;
    size_t whole = 0;

    /*
     * Check every record first, so that a bad one leaves the batch as it
     * was. It costs a second look at headers that are in the cache.
     */
    for (at = 0; at < len; at += record.size) {
        if (!record_at(records + at, len - at, &record)) {
            *count = whole;
            return ROCKSDB_HASKELL_MALFORMED;
        }
        whole += 1;
    }

    for (at = 0; at < len; at += record.size) {
        /* cannot fail: the same bytes passed above */
        if (!record_at(records + at, len - at, &record)) {
            break;
        }
        add_record(batch, &record);
    }
    *count = whole;
    return ROCKSDB_HASKELL_OK;
}

/* The state of a merge operator is its name, which it owns. */
static void merge_operator_destroy(void *state)
{
    free(state);
}

static const char *merge_operator_name(void *state)
{
    return state;
}

int rocksdb_haskell_options_set_merge_operator(
    rocksdb_options_t *options,
    const char *name,
    rocksdb_haskell_full_merge_t full_merge,
    rocksdb_haskell_partial_merge_t partial_merge)
{
    size_t size = strlen(name) + 1;
    char *copy = malloc(size);
    rocksdb_mergeoperator_t *merge_operator;

    if (copy == NULL) {
        return ROCKSDB_HASKELL_NO_MEMORY;
    }
    memcpy(copy, name, size);
    /*
     * No function to delete a value: RocksDB then frees with free() what
     * the two functions return.
     */
    merge_operator = rocksdb_mergeoperator_create(
        copy,
        merge_operator_destroy,
        full_merge,
        partial_merge,
        NULL,
        merge_operator_name);
    /* the options take the operator, and destroy it when nothing uses it */
    rocksdb_options_set_merge_operator(options, merge_operator);
    return ROCKSDB_HASKELL_OK;
}
