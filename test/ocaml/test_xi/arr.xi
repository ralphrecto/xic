main (args : int[][]) {
    println (string_of_2array (foo1 ()));
    println (string_of_3array (foo2 ()));
    println (string_of_1array (foo3 ()));
    println (string_of_1array (foo4 ()));
    println (string_of_1array (foo5 ()));
    println (string_of_1array (foo6 ()));
    println (string_of_2array (foo7 ()));
    println (string_of_int    (foo8 ()));
    println (string_of_1array (foo9 ()));
    println (string_of_1array (foo10 ()));
    println (string_of_1array (foo11 ()));
    println (string_of_1array (bar ()));

    // Testing length()
    println (string_of_int    (foo13 ()));
    println (string_of_int    (foo14 ()));
    println (string_of_int    (foo15 ()));
    println (string_of_int    (foo16 ()));
    
    // Testing indexing
    println (string_of_int    (foo17 ()));
    println (string_of_1array (foo18 ()));
    println (string_of_int    (foo19 ()));
}

foo1 () : int[][] {
    return {{}, {}, {}, {}};
}

foo2 () : int[][][] {
    return {{{}}, {{1,2},{3,4}}, {{1}, {2}}};
}

foo3 () : int[] {
    return {} + {};
}

foo4 () : int[] {
    return {1} + {};
}

foo5 () : int[] {
    return {} + {0};
}

foo6 () : int[] {
    return {1, 1, 1} + {0, 1};
}

foo7 () : int[][] {
    return {} + {{1,2} + {3,4}} + {{1}};
}

foo8 () : int {
    return {3, 2, 1}[0];
}

foo9 () : int[] {
    return {{}}[0];
}

foo10 () : int[] {
   return {{1,2,3},{4,5}}[0] + {6, 7};
}

foo11 () : int[] {
    return "Hello" + {13, 10};
}

bar (): int[] {
    return {1, 2, 3} + foo12();
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

foo17 (): int {
    return {1}[0];
}

foo18 (): int[] {
    return {{0}, {1}}[0];
}

foo19 (): int {
    return {{1, 2, 3}, {4, 5, 6}, {7, 8, 9}, {0}}[1][2];
}
