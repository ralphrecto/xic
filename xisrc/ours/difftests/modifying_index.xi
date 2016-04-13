foo(x: int[]) : int {
    println("foo");
    x[1] = x[1] + 1
    return 0
}

main(_: int[][]) {
    x: int[] = {0, 1}
    println(string_of_1array(x));
    x[foo(x)] = x[0] + 1
    println(string_of_1array(x));
}
