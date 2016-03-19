main () {
    print (string_of_1array (foo ()));
}

foo () : int[] {
    return {1, 1, 1} + {0, 1};
}
