main (args : int[][]) {
    // Testing length()
    println (string_of_int    (foo13 ()));
    println (string_of_int    (foo14 ()));
    println (string_of_int    (foo15 ()));
    println (string_of_int    (foo16 ()));
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
