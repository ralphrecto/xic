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

    println(string_of_1array(x));
    println(string_of_1array(y));
    x[y[0]] = foo(y)
    println(string_of_1array(x));
    println(string_of_1array(y));
}
