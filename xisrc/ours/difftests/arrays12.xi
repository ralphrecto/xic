foo(x: int[]) {
    x[0] = 1
}

main(_: int[][]) {
    x: int[] = {1, 2, 3, 4, 5, 6}
    foo(x)
    println(string_of_1array(x))
}
