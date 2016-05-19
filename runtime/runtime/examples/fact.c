/**
 Interface to the Xi runtime
*/

#include "libxi.h"

/**
 Test program
*/
xiint XI(factorial)(xiint i) {
    if (i <= 1)
        return 1;
    else
        return i * XI(factorial)(i-1);
}

xiint prompt[] = { 7, 'N', 'u', 'm', 'b', 'e', 'r', '?' };
xiint is[]   = { 5, '!', ' ', 'i', 's', ' '};

void XI(main_paai)(xistring arg[]) {
    xistring input, output;

    while (!XI(eof_b)()) {
        XI(print_pai)(prompt + 1);
        input = XI(readln_ai)();

        // Convert to integer
        xibool ok;
        xiint num = XI(parseInt_t2ibai)(input);
        GET2NDRESULT(ok);

        if (ok) {
            xiint val = XI(factorial)(num);

            output  = XI(unparseInt_aii)(num);
            XI(print_pai)(is + 1);
            output  = XI(unparseInt_aii)(val);
            XI(println_pai)(output);
        }
    }
}
