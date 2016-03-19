main () {
    print (string_of_2array (foo1 ()));
    print (string_of_3array (foo2 ()));
    print (string_of_1array (foo3 ()));
    print (string_of_1array (foo4 ()));
    print (string_of_1array (foo5 ()));
    print (string_of_1array (foo6 ()));
    print (string_of_2array (foo7 ()));
    print (string_of_int    (foo8 ()));
    print (string_of_1array (foo9 ()));
    print (string_of_1array (foo10 ()));
    print (string_of_1array (foo11 ()));
    print (string_of_1array (bar ()));
    print (string_of_int    (foo13 ()));
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
