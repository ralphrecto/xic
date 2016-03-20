main (args : int[][]) {
    // Testing length()
    print (string_of_int    (foo13 ()));
    print (string_of_int    (foo14 ()));
    print (string_of_int    (foo15 ()));
    print (string_of_int    (foo16 ()));
}

foo12 () : int[] {
    return {10, 11};
}

foo13 (): int {
    return length({{1,2,3}, {4}});
}

foo14 (): int {
    return length({});
}

foo15 (): int {
    return length({{}});
}

foo16 (): int {
    return length(foo12());
}
