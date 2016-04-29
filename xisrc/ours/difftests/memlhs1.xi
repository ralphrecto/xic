foo(l: int[][], y: int[], n: int) : int {
    l[0] = y
    return 42
}

main(_: int[][]) {
    x: int[]   = {0, 2, 4}
    y: int[]   = {1, 3, 5}
    l: int[][] = {x}
    l[0][0] = foo(l, y, 42)

    print_1array(x);
    print_1array(y);
    print_2array(l);
}
