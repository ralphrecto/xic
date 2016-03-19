main () {
    print (string_of_1array (foo()));
}

foo () : int[] {
    return "Hello" + {13, 10};
}
