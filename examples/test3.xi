foo(): bool, int {
    expr: int = 1 - 2 * 3 * -4
    pred: bool = true & true | false;
    x:int = expr + pred;
    if (expr <= 47) { }
    else pred = !pred
    if (pred) { expr = 59 }
    return pred, expr;
}

bar() {
    _, i: int = foo()
    b: int[i][];
    b[0] = {1, 0}
}
