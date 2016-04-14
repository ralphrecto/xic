#ifdef __clang__
## Mac OS X
#define FUNC(x) _##x
#define ARG1() %rdi
#define ARG2() %rsi
#define ARG3() %rdx
#define ARG4() %rcx
#define ARG5() %r8
#define ARG6() %r9
#define INITFUNC() .mod_init_func
#define GCFUNC(x) _##x
#endif

#ifdef linux
#define FUNC(x) _##x
#define ARG1() %rdi
#define ARG2() %rsi
#define ARG3() %rdx
#define ARG4() %rcx
#define ARG5() %r8
#define ARG6() %r9
#define INITFUNC() .section	.ctors
#define GCFUNC(x) x
#endif

#ifdef __CYGWIN__
#define FUNC(x) x
#define ARG1() %rcx
#define ARG2() %rdx
#define ARG3() %r8
#define ARG4() %r9
#define INITFUNC() .section	.ctors,"w"
#define GCFUNC(x) x
#endif
