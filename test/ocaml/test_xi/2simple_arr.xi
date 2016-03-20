main (args : int[][]) {
    print (string_of_2array (foo1 ()));
    print (string_of_3array (foo2 ()));
    print (string_of_1array (foo3 ()));
    print (string_of_1array (foo4 ()));
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
