main () : int[] {
    return {1, 2, 3} + foo();
}

foo () : int[] {
    return {10, 11};
}
