/**
 Some exports to help write Xi bindings
*/
#ifndef LIBXI_H
#define LIBXI_H

#include "multret.h"
#include <stdint.h>

typedef int64_t xiint;
typedef int64_t xibool;
typedef xiint *xistring;

#define xilength(a) *(xiint *)((a)-1)

#ifdef __cplusplus
#define XI_EXPORT extern "C"
#else
#define XI_EXPORT
#endif

#if !defined(__CYGWIN__) && !defined(__APPLE__)
#define XI(x) _I ## x
#else
#define XI(x) I ## x
/* On Cygwin/Windows (and apparently OS X) the compiler adds _ itself to
   everything; so we don't need one of our own */
#endif

// Main allocation hook
XI_EXPORT void * XI(_alloc_i)(xiint);

// Registers a finalizer for a given object
typedef void Finalizer(void*, void*);
XI_EXPORT void registerFinalizer(void*, Finalizer*);

extern xiint XI(parseInt_t2ibai)(xistring);
extern xistring XI(readln_ai)(void);
extern xistring XI(unparseInt_aii)(xiint);
extern xibool XI(eof_b)(void);
extern void XI(println_pai)(xistring);
extern void XI(print_pai)(xistring);


#endif
// kate: indent-width 4; replace-tabs on; tab-width 4; space-indent on;
