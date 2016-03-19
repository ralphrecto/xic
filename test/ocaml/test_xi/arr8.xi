main () {
    print (string_of_int (foo ()));
}

foo () : int {
    return {3, 2, 1}[0];
}
