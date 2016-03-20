use io
use conv

foo(a:int): bool, int {
    println(unparseInt(a));
    expr: int = 1 - 2 * 3 * -4
    pred: bool = true & true | false;
    if (expr <= 47) { }
    else pred = !pred
    if (pred) { expr = 59 }
    return pred, expr;
}

main(a:int[][]) {
    _, i: int = foo(5);
    println(unparseInt(i));
    println("making b...");
    b: int[i][];
    println("indexing b...");
    b[0] = {1, 0}
    println("hello");
}
