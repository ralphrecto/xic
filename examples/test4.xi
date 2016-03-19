use io

foo(): bool, int {
    expr: int = 1 - 2 * 3 * -4
    pred: bool = true & true | false;
    if (expr <= 47) { }
    else pred = !pred
    if (pred) { expr = 59 }
    print("HELLO WORLD");
    return pred, expr;
}

bar() {
    _, i: int = foo()
    b: int[i][];
    b[0] = {1, 0}
}
