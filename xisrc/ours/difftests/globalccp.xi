global: int = 0

foo() {
    global = 42
}

main(_: int[][]) {
    global = 0
    foo()
    x: int = global
    if (x == 42) {
        println("yay!")
    } else {
        check(false)
    }
}
