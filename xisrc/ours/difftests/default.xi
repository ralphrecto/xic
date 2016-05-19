defaultTest1() {
    x: int
    y: bool
    xs: int[]
    ys: int[]
    zs: int[1][]
    println(string_of_int(x))
    println(string_of_bool(y))
    check(xs == null)
    check(ys == null)
    check(xs == ys)
    check(zs[0] == null)
    check(zs[0] == xs)
}

main(_: int[][]) {
    defaultTest1()
}
