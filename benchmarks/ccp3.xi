main(args: int[][]) {
    x: int = 2
    y: int = x + x * x + x
    if (y > y + 1) {
        y = y + y
    } else {
        y = 2 * y
    }
    x = y * y
}
