#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <limits.h>

typedef struct input_state {
    char heights[5];
    int state;
} input_state_t;

typedef struct keys_locks {
    size_t len;
    size_t cap;
    input_state_t* inputs;
} keys_locks_t;

typedef struct keys_locks_val {
    size_t klen;
    size_t llen;
    size_t* keys;
    size_t* locks;
} kl_vals_t;

kl_vals_t convertKeysAndLocks(keys_locks_t kl) {
    kl_vals_t v = {0};
    v.keys = (size_t*) malloc(sizeof(size_t) * kl.len);
    v.locks = (size_t*) malloc(sizeof(size_t) * kl.len);

    char buf[8] = {0};

    for (size_t i = 0; i < kl.len; i++) {
        memset(buf, 0, 8);
        memcpy(buf, kl.inputs[i].heights, 5);
        switch (kl.inputs[i].state) {
            case 1:
                // is a key
                v.keys[v.klen++] = *((size_t*) buf) - 0x0101010101;
                // printf("heights to value: 0x%010lx\n", *((size_t*) buf) - 0x0101010101);
                break;
            case 2:
                // is a lock
                v.locks[v.llen++] = *((size_t*) buf);
                // printf("heights to value: 0x%010lx\n", *((size_t*) buf));
                break;
            default:
                printf("invalid input state %d\n", kl.inputs[i].state);
                break;
        }
    }
    return v;
}

input_state_t* newKeyOrLock(keys_locks_t* kl) {
    if (kl->cap == 0) {
        kl->cap = 64;
        kl->inputs = (input_state_t*) malloc(sizeof(input_state_t) * kl->cap);
    } else if (kl->len >= kl->cap) {
        kl->cap *= 2;
        input_state_t* ptr = (input_state_t*) realloc(kl->inputs, sizeof(input_state_t) * kl->cap);
        if (ptr == NULL) return NULL;
        kl->inputs = ptr;
    }
    size_t idx = kl->len;
    kl->inputs[idx].state = 0;
    kl->len++;
    return &kl->inputs[idx];
}

void initState(input_state_t* st, char* row) {
    if (strncmp(row, ".....", 5) == 0) {
        st->state = 1;
    }
    else if (strncmp(row, "#####", 5) == 0) {
        st->state = 2;
    }
    else {
        printf("invalid init row: %s", row);
        st->state = -1;
    }
}

void addHeights(input_state_t* st, char* row) {
    for (int i = 0; i < 5; ++i) {
        switch (row[i]) {
            case '.':
                break;
            case '#':
                st->heights[i]++;
                break;
            default:
                // invalid
                printf("invalid row value %x\n", row[i]);
                break;
        }
    }
}

void feedInput(input_state_t* st, char* row) {
    switch (st->state) {
        case 0:
            // uninitialized
            initState(st, row);
            break;
        case 1:
            // is a key
            addHeights(st, row);
            break;
        case 2:
            // is a lock
            addHeights(st, row);
            break;
        default:
            // invalid
            printf("invalid state %d\n", st->state);
            break;
    }
}

keys_locks_t readInput(FILE* f) {
    char buf[64];

    keys_locks_t kl = {0};
    input_state_t* cur = newKeyOrLock(&kl);
    kl.cap = 64;
    while (fgets(buf, 64, f)) {
        if (strcmp(buf, "\n") == 0) {
            // next input
            cur = newKeyOrLock(&kl);
        }
        else {
            feedInput(cur, buf);
        }
    }
    return kl;
}

int getFittingKeysLocks(kl_vals_t v) {
    int fits = 0;
    for (size_t l = 0; l < v.llen; ++l) {
        size_t lock = v.locks[l];
        for (size_t k = 0; k < v.klen; ++k) {
            size_t key = v.keys[k];
            size_t lk = lock + key + 0x0a0a0a0a0a;
            // printf("lock+key 0x%010lx\n", lk & 0xf0f0f0f0f0);
            if ((lk & 0xf0f0f0f0f0) == 0) {
                fits++;
            }
        }
    }
    return fits;
}

int main(int argc, char** argv) {
    assert(argc == 2);
    FILE* f = fopen(argv[1], "r");
    keys_locks_t kl = readInput(f);
    fclose(f);

    kl_vals_t klv = convertKeysAndLocks(kl);
    int ft = getFittingKeysLocks(klv);
    printf("key/lock pairs that fit: %d\n", ft);
}
