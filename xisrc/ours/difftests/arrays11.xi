foo(x: int[]) {
}

main(_: int[][]) {
    x: int[] = {
        1,   2,   3,   4,   5,   6,   7,   8,   9,
        11,  12,  13,  14,  15,  16,  17,  18,  19,
        21,  22,  23,  24,  25,  26,  27,  28,  29,
    }
    print_1array(x)
    foo(x)
    println(string_of_1array(x))
}
