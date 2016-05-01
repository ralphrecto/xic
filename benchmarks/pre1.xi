use conv
use io

nums(): int, int, int, int, int, int {
    return 1, 2, 3, 4, 5, 6
}

main(args: int[][]) {
    a:int, b:int, c:int, d:int, e:int, f:int = nums()
    x: int = a*b*c*d*e*f*a*b*c*d*e*f
    n: int = 0
    while (n < 10000000) {
        x = a*b*c*d*e*f*a*b*c*d*e*f
        x = a*b*c*d*e*f*a*b*c*d*e*f
        x = a*b*c*d*e*f*a*b*c*d*e*f
        x = a*b*c*d*e*f*a*b*c*d*e*f
        x = a*b*c*d*e*f*a*b*c*d*e*f
        n = n + 1
    }
}
