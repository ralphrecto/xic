main(args: int[][]) {
    a:int=1; b:int=2; c:int=3; d:int=4; e:int=5; f:int=6;
    x: int = a*b*c*d*e*f*a*b*c*d*e*f
    n: int = 0
    while (n < 10000000) {
        x = a*b*c*d*e*f*a*b*c*d*e*f
        x = a*b*c*d*e*f*a*b*c*d*e*f
        x = a*b*c*d*e*f*a*b*c*d*e*f
        x = a*b*c*d*e*f*a*b*c*d*e*f
        x = a*b*c*d*e*f*a*b*c*d*e*f
        x = a*b*c*d*e*f*a*b*c*d*e*f
        x = a*b*c*d*e*f*a*b*c*d*e*f
        x = a*b*c*d*e*f*a*b*c*d*e*f
        n = n + 1
    }
}
