/**
 An implementation of the Xi standard runtime library.

 Note: string functions are NOT Unicode clean to avoid dealing with portability
 issues --- ideally, Xi strings should be viewed as UCS-4, but they are treated
 as expanded out local 8-bit encoding
*/

#include "libxi.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>
#include <inttypes.h>

#include "../gc-7.2/include/gc.h"

/**
 Core runtime
*/

void * XI(_alloc_i)(xiint size) {
    return GC_malloc(size);
}

void registerFinalizer(void* object, Finalizer* fin) {
    GC_register_finalizer_ignore_self(object, fin, 0, 0, 0);
}

void XI(_outOfBounds_p)(void) {
    fprintf(stderr, "Array index out of bounds\n");
    abort();
}

// Internal helper for making arrays
static void* mkArray(int bytes, int cells) {
    xiint *memory = XI(_alloc_i)(bytes + sizeof(xiint));
    memory[0] = cells;
    return memory + 1;
}

// Helper: C string to XI string
static xistring mkString(const char* in) {
    int  c;
    int  len = strlen(in);
    xistring out = mkArray(len * sizeof(xiint), len);

    for (c = 0; c < len; ++c)
        out[c] = (unsigned char)in[c];
    return out;
}

extern void XI(main_paai)(xistring[]);

int main(int argc, char *argv[]) {

    GC_INIT();

    // GC setup. We do point in the "middle" of arrays.
    GC_set_all_interior_pointers(1);

    // Create arguments array.
    xistring *args = mkArray(sizeof(xiint *) * argc, argc);
    int c;
    for (c = 0; c < argc; ++c)
        args[c] = mkString(argv[c]);

    // transfer to program's main
    XI(main_paai)(args);
    return 0;
}


/**
 I/O module
*/

void XI(print_pai) (xistring str) {
    int c;
    int len = str[-1];
    for (c = 0; c < len; ++c)
        fputc(str[c], stdout);
}

void XI(println_pai) (xistring str) {
    XI(print_pai)(str);
    fputc('\n', stdout);
}

xistring XI(readln_ai) (void) {
    int len = 0, max = 128;
    xistring res = (xistring)malloc((max+1) * sizeof(xiint));
    res = res + 1;
    res[-1] = len;

    for (;;) {
        int c = getchar();
        if (c == -1) break;
        if (c == '\n') break;
        if (len == max) {
            max *= 2;
            xistring nres = (xistring)malloc((max+1) * sizeof(xiint));
            memcpy(nres, &res[-1], (len + 1) * sizeof(xiint));
            free(res);
            res = nres;
        }
        res[len++] = c;
    }

    res[-1] = len;

    return res;
}

xiint XI(getchar_i) (void) {
    // ### behavior on eof is unspecified
    return fgetc(stdin);
}

xiint XI(eof_b) (void) {
    return feof(stdin) ? 1 : 0;
}

/**
 Conv module
*/

xiint XI(parseInt_t2ibai) (xistring str) {
    // ### should this worry about overflow?
    int c;
    int len = str[-1];
    int neg = 0;
    xiint num = 0;
    xiint ok = 0;

    if (!len) {
        SET2NDRESULT(ok);
        return num;
    }

    if (str[0] == '-')
        neg = 1;

    for (c = neg; c < len; ++c) {
        if (str[c] >= '0' && str[c] <= '9') {
            num = 10*num + (str[c] - '0');
        } else {
            num = 0;
            SET2NDRESULT(ok);
            return num; // returning (0, false);
        }
    }

    ok = 1;
    if (neg) num = -num;
    SET2NDRESULT(ok);
    return num;
}

xistring XI(unparseInt_aii) (xiint in) {
    char buf[32]; // more than enough to represent 64-bit numbers

#if defined(WINDOWS) || defined(WIN32)
    sprintf(buf, "%I64d", in);
#else
    sprintf(buf, "%lld", in);
#endif

    return mkString(buf);
}

/**
 Assert module
*/
void XI(assert_pb) (xiint cond) {
    if (!cond) {
        fprintf(stderr, "Assertion failed\n");
        abort();
    }
}

/**
 Timer module. This ab(uses) the long[] type for
 GC'd opaque objects.
*/
struct timeval* XI(getTimestamp_ai) (void) {
    struct timeval* tStamp = mkArray(sizeof(struct timeval), 0);
    gettimeofday(tStamp, 0);
    return tStamp;
}

xiint XI(timestampDifference_iaiai)(xiint* l, xiint* r) {
    struct timeval* lTime = (struct timeval*)l;
    struct timeval* rTime = (struct timeval*)r;

    xiint secondsDiff = lTime->tv_sec  - rTime->tv_sec;
    xiint usecDiff  = lTime->tv_usec - rTime->tv_usec; // micro is 1e6, so the range is fine.

    long combinedDiff   = usecDiff + secondsDiff * 1000000L;
    return (xiint)(combinedDiff/1000);
}


// kate: indent-width 4; replace-tabs on; tab-width 4; space-indent on;
// vim: sw=4 ts=4 et
