/**
 A simple demangling filter for Xi. (Similar to c++filt). Very basic,
 assuming the current locale is right and 8-bit things work and so on.

 Doing this in C is painful, but I want to reduce installation footprint for
 the students...
*/

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "../libxi/libxi.h"


/**
 An (inefficient) helper for string concatenation --- takes 2
 heap allocated strings, creates one with their combination
 and deletes the inputs.

 As a special case, if s1 is 0, it just returns s2 unchanged.
*/
char* stringCombine(char* s1, char* s2) {
    if (s1) {
        int l1 = strlen(s1);
        int l2 = strlen(s2);
        char* out = malloc(l1 + l2 + 1);
        memmove(out, s1, l1);
        memmove(out + l1, s2, l2);
        out[l1 + l2] = '\0';
        free(s1);
        free(s2);
        return out;
    } else {
        return s2;
    }
}

/**
 Tries to demangle a name starting from *posPtr.
 If successful, returns the name and *posPtr is updated to point to
 next input character (may be 0). The newly returned name is heap-allocated

 If not, 0 is returned and *posPtr isn't changed
*/
char* tryDemangleName(char** posPtr) {
    char* startPos = *posPtr;
    char* pos, *endPos;
    char *out, *outPos;

    // First pass: try to see how much matches
    for (pos = startPos; *pos; ++pos) {
        // If we're alpha, we're OK
        if (isalpha(*pos))
            continue;

        // Nothing else other than alpha can be valid for first character!
        if (pos == startPos)
            break;

        // numbers are ok if not first.
        if (isdigit(*pos))
            continue;

        // as are escaped _ are OK
        if (*pos == '_' && pos[1] == '_') {
            ++pos; // skip both chars.
            continue;
        }

        // Didn't find anything.
        break;
    }

    // Note: we stopped on the first invalid character (maybe 0);
    // so length is pos - startPos
    if (pos == startPos)
        return 0;

    out = (char*)malloc(pos - startPos + 1);

    // Copy down unescaping
    endPos = pos;
    for (outPos = out, pos = startPos; pos < endPos; ++pos, ++outPos) {
        *outPos = *pos;
        if (*pos == '_')
            ++pos; // skip the extra escape
    }
    *outPos = '\0';
    *posPtr = endPos;
    return out;
}

// Decode a numbers, returning 0 if it fails, as 0 is not expected to
// be valid
int tryDemangleNumber(char** posPtr) {
    char* pos = *posPtr;
    int num = 0;

    // Read in number of elements
    while (isdigit(*pos)) {
        num = 10*num + (*pos - '0');
        ++pos;
    }
    *posPtr = pos;
    return num;
}

/**
 Decodes types --- same interface as above
*/
char* tryDemangleType(char** posPtr) {
    char* pos = *posPtr;
    if (*pos == 'i') {
        ++*posPtr;
        return strdup("int");
    }

    if (*pos == 'b') {
        ++*posPtr;
        return strdup("bool");
    }

    if (*pos == 'o') {
        int   length, out;
        char* str;
        ++pos;
        length = tryDemangleNumber(&pos);
        if (!length)
            return 0;

        str = malloc(length + 1);
        for (out = 0; out < length; ++pos, ++out) {
            if (*pos == '_') {
                if (pos[1] != '_') {
                    free(str);
                    return 0;
                }
                ++pos;
            }

            str[out] = *pos;
        }
        str[out] = '\0';

        *posPtr = pos;
        return str;
    }

    if (*pos == 'a') {
        char* elementType;
        ++pos;
        // Try to read component type.
        elementType = tryDemangleType(&pos);
        if (elementType) {
            *posPtr = pos;
            return stringCombine(elementType, strdup("[]"));
        }
    }

    if (*pos == 't') {
        int numComponents;
        ++pos;
        numComponents = tryDemangleNumber(&pos);

        if (numComponents) { // not t0 or plain t, which are invalid.
            char* type = strdup("(");
            int c;
            for (c = 0; c < numComponents; ++c) {
                char* typeComp = tryDemangleType(&pos);
                if (!typeComp) {
                    free(type);
                    return 0;
                }

                if (c)
                    type = stringCombine(type, strdup(", "));
                type = stringCombine(type, typeComp);
            }

            type = stringCombine(type, strdup(")"));

            *posPtr = pos;
            return type;
        } // if numCompontents
    } // if tuple

    return 0;
}

/**
 Tries to demangle Xi initialization/info symbols.
*/
char* tryDemangleXiSetup(char** posPtr) {
    // Possible things here are vt_name, size_name, init_name.
    char* pos = *posPtr;
    char* cfy, *cname;

    cfy = tryDemangleName(&pos);
    if (!cfy)
        return 0;

    if (*pos != '_') {
        free (cfy);
        return 0;
    }

    ++pos;

    cname = tryDemangleName(&pos);
    if (!cname) {
        free(cfy);
        return 0;
    }

    // Well, we got the rough form, now see if the first half is OK
    const char* prefix = 0;
    if (!strcmp(cfy, "vt"))
        prefix = "Xi vtable for ";

    if (!strcmp(cfy, "size"))
        prefix = "Xi size of ";

    if (!strcmp(cfy, "init"))
        prefix = "Xi init of ";

    free(cfy);

    if (prefix) {
        *posPtr = pos;
        return stringCombine(strdup(prefix), cname);
    } else {
        free(cname);
        return 0;
    }
}

/*
 Tries to demangle Xi global symbols.
*/
char* tryDemangleXiGlobal(char** posPtr) {
    // _I_g_<name>_<type>..
    char* pos = *posPtr;
    char* cfy, *name, *type;

    cfy = tryDemangleName(&pos);
    if (!cfy)
        return 0;

    if (*pos != '_' || strcmp(cfy, "g")) {
        free (cfy);
        return 0;
    }
    ++pos;
    free(cfy);

    name = tryDemangleName(&pos);
    if (!name)
        return 0;

    if (*pos != '_') {
        free(name);
        return 0;
    }
    ++pos;

    type = tryDemangleType(&pos);
    if (!type) {
        free(name);
        return 0;
    }

    *posPtr = pos;
    return stringCombine(stringCombine(name, strdup(": ")), type);
}

/**
 Decodes entire signatures --- same interface as above
*/
char* tryDemangleSymbol(char** posPtr) {
    char* pos = *posPtr;
    char* decl, *retInfo, *argInfo;
    int firstArg, runtime = 0;

    // _I
    if (pos[0] != '_' || pos[1] != 'I')
        return 0;

    pos += 2;

    // here, we may have _ in case of runtime support symbols,
    // like _I_alloc_i
    if (pos[0] == '_') {
        char* ixInfo;

        ++pos;
        ixInfo = tryDemangleXiSetup(&pos);
        if (ixInfo) {
            *posPtr = pos;
            return ixInfo;
        }

        ixInfo = tryDemangleXiGlobal(&pos);
        if (ixInfo) {
            *posPtr = pos;
            return ixInfo;
        }

        // Runtime function... largely uses the normal path below.
        runtime = 1;
    }

    // function name
    decl = tryDemangleName(&pos);
    if (!decl)
        return 0;

    // add annotation for runtime functions
    if (runtime)
        decl = stringCombine(strdup("[xiruntime]"), decl);

    // _
    if (*pos != '_') {
        free(decl);
        return 0;
    }
    ++pos;

    // return type or procedure
    if (*pos == 'p') {
        retInfo = strdup("");
        ++pos;
    } else {
        retInfo = tryDemangleType(&pos);
        if (!retInfo) {
            free(decl);
            return 0;
        }

        retInfo = stringCombine(strdup(": "), retInfo);
    }

    // Now arguments. This is actually a bit ambiguous while
    // filtering, since we may not have a bondary, but should be OK
    // with actual error messages
    decl = stringCombine(decl, strdup("("));

    firstArg = 1;
    while ((argInfo = tryDemangleType(&pos))) {
        if (!firstArg)
            decl = stringCombine(decl, strdup(", "));
        decl = stringCombine(decl, argInfo);
        firstArg = 0;
    }

    decl = stringCombine(decl, strdup(")"));
    decl = stringCombine(decl, retInfo);
    *posPtr = pos;
    return decl;
}

#if defined(WINDOWS) || defined(WIN32)
ssize_t getline(char **linep, size_t *max, FILE *s) {
    ssize_t len = 0;
    if (*max == 0)
        *linep = malloc(*max = 128);
    for (;;) {
        int c = getchar();
        if (c == -1) break;
        if (c == '\n') break;
        if (len == *max) {
            *max *= 2;
            *linep = realloc(*linep, *max);
        }
        (*linep)[len++] = c;
    }
    return len;
}
#endif

int main() {
    do {
        char *pos, *demangled, *line = NULL;
        size_t size = 0;

        ssize_t len = getline(&line, &size, stdin);
        if (len == -1) goto freeline;
        pos  = line;
        while (*pos) {
            demangled = tryDemangleSymbol(&pos);
            if (demangled) {
                printf("%s", demangled);
                free(demangled);
            } else {
                fputc(*pos, stdout);
                ++pos;
            }
        }

        freeline:
        free(line);
    }
    while (!feof(stdin) && !ferror(stdin));

    return 0;
}

// kate: indent-width 4; replace-tabs on; tab-width 4; space-indent on;
