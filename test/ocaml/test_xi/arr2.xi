main () {
    return (print (string_of_3array (foo())));
}

foo () : int[][][] {
    return {{{}}, {{1,2},{3,4}}, {{1}, {2}}};
}
