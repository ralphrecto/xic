mangle_first(x: int[]) {
    x[0] = 42
}

main(_: int[][]) {
    x: int[] = {0, 1, 2, 3}
    println(string_of_1array(x))
    mangle_first(x)
    println(string_of_1array(x))
}
