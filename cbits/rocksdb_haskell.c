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
         */
        if (read > 0 && (bytes >= max_bytes || key_len + value_len > max_bytes - bytes)) {
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
