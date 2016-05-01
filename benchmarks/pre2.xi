main(args: int[][]) {
    a:int=1; b:int=2; c:int=3; d:int=4; e:int=5; f:int=6;
    x:int = 0;
    i:int = 0;
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
