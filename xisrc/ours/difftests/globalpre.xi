global: int = 0

bar() : int {
    return 76
}

foo() {
    global = 42
}

main(_: int[][]) {
    x: int = 0
    if (bar() > 10) {
        foo()
        x = global + global
    } else {
        x = global + global
    }
    x = global + global
    check(x == 84)
}
