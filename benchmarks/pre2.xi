use conv
use io
nums(): int, int, int, int, int, int {
    return 1, 2, 3, 4, 5, 6
}
main(args: int[][]) {
    a:int, b:int, c:int, d:int, e:int, f:int = nums(); x:int = 0; i:int = 0;
    while(i < 50000000) {
        x = (a * b)
        if (a > 10) {
            x = (a * b) * (a * b)
        } else {
            x = (a * b) * (a * b) * (a * b)
        }
        x = (a * b) * (a * b) * (a * b) * (a * b)
        i = i + 1
    }
}
