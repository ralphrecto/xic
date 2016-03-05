foo() {
    x:int = 2;
    z:int;
    b: bool, i:int = f(x);
    s: int[] = "Hello";
}

f(i:int): bool, int {
    return i < 2, i + 2
}