/*
 * A merge operator for the tests of rocksdb-haskell-jprupp: it joins byte
 * strings with commas.
 *
 * The value of a key is the value it had, if any, then its merge records
 * from the oldest to the newest, with a comma between each two. So the
 * value shows which records were merged and in which order, which a sum
 * would not. A record of the single byte '!' makes a merge fail, to see
 * what RocksDB does then.
 *
 * The functions have the types of rocksdb_haskell_full_merge_t and
 * rocksdb_haskell_partial_merge_t in cbits/rocksdb_haskell.h.
 */

#include <stddef.h>
#include <stdlib.h>
#include <string.h>

/* Report a failure the way the header says to. */
static char *fail(unsigned char *success, size_t *new_value_length)
{
    *success = 0;
    *new_value_length = 0;
    return NULL;
}

/*
 * Join a first byte string, if there is one, and some more, with commas.
 */
static char *join(
    const char *first,
    size_t first_length,
    const char *const *list,
    const size_t *lengths,
    int count,
    unsigned char *success,
    size_t *new_value_length)
{
    size_t size = first == NULL ? 0 : first_length;
    size_t used = 0;
    char *joined;
    int i;

    for (i = 0; i < count; i++) {
        if (lengths[i] == 1 && list[i][0] == '!') {
            return fail(success, new_value_length);
        }
        /* a comma before every part but the first */
        size += lengths[i] + (first != NULL || i > 0 ? 1 : 0);
    }
    /* malloc may return NULL for no bytes, which is not a failure here */
    joined = malloc(size > 0 ? size : 1);
    if (joined == NULL) {
        return fail(success, new_value_length);
    }
    if (first != NULL && first_length > 0) {
        memcpy(joined, first, first_length);
        used = first_length;
    }
    for (i = 0; i < count; i++) {
        if (first != NULL || i > 0) {
            joined[used] = ',';
            used += 1;
        }
        if (lengths[i] > 0) {
            memcpy(joined + used, list[i], lengths[i]);
            used += lengths[i];
        }
    }
    *success = 1;
    *new_value_length = used;
    return joined;
}

char *rocksdb_haskell_test_join_full(
    void *state,
    const char *key,
    size_t key_length,
    const char *existing_value,
    size_t existing_value_length,
    const char *const *operands_list,
    const size_t *operands_list_length,
    int num_operands,
    unsigned char *success,
    size_t *new_value_length)
{
    (void)state;
    (void)key;
    (void)key_length;
    return join(
        existing_value,
        existing_value_length,
        operands_list,
        operands_list_length,
        num_operands,
        success,
        new_value_length);
}

char *rocksdb_haskell_test_join_partial(
    void *state,
    const char *key,
    size_t key_length,
    const char *const *operands_list,
    const size_t *operands_list_length,
    int num_operands,
    unsigned char *success,
    size_t *new_value_length)
{
    (void)state;
    (void)key;
    (void)key_length;
    return join(NULL, 0, operands_list, operands_list_length, num_operands, success, new_value_length);
}

/* A partial merge that always fails: RocksDB then keeps the records. */
char *rocksdb_haskell_test_no_partial(
    void *state,
    const char *key,
    size_t key_length,
    const char *const *operands_list,
    const size_t *operands_list_length,
    int num_operands,
    unsigned char *success,
    size_t *new_value_length)
{
    (void)state;
    (void)key;
    (void)key_length;
    (void)operands_list;
    (void)operands_list_length;
    (void)num_operands;
    return fail(success, new_value_length);
}
