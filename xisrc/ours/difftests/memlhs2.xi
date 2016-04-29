foo(y: int[]) : int {
    y[0] = 1
    return 42
}

id(x: int[]) : int[] {
    return x
}

main(_: int[][]) {
    x: int[] = {0, 1}
    y: int[] = {0}

    print_1array(x);
    print_1array(y);
    x[y[0]] = foo(y)
    print_1array(x);
    print_1array(y);
}
