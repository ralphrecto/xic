foo(l: int[][], y: int[], n: int) : int {
    l[0] = y
    return 42
}

main(_: int[][]) {
    x: int[]   = {0, 2, 4}
    y: int[]   = {1, 3, 5}
    l: int[][] = {x}
    l[0][0] = foo(l, y, 42)

    println(string_of_1array(x));
    println(string_of_1array(y));
    println(string_of_2array(l));
}
