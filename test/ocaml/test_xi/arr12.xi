main () {
    print (string_of_1array (bar()));
}

bar (): int[] {
    return {1, 2, 3} + foo();
}

foo () : int[] {
    return {10, 11};
}
