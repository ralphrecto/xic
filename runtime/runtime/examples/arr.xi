use io

main(args: int[][]) {
    argc: int = length(args)
    a: int = 0
    while (a < argc) {
        println(args[a])
        a = a + 1
    }
}
