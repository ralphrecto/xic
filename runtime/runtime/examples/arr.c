#include "libxi.h"

void XI(main_paai)(xistring arg[]) {
    xiint argc = xilength(arg);
    xiint a;
    for (a = 0; a < argc; a++) {
        XI(println_pai)(arg[a]);
    }
}
