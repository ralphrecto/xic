main (args : int[][]) {
    println (string_of_2array (foo1 ()));
    println (string_of_3array (foo2 ()));
    println (string_of_1array (foo3 ()));
    println (string_of_1array (foo4 ()));
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
    x:int[] = {1,2} + {};
    println(string_of_int (length(x)));
    println(string_of_1array(x));
    return x;
}
