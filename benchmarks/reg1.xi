main(args: int[][]) {
    i: int = 0
    while (i < 20 * 1000 * 1000) {
        a1:int = 1; b1:int = 2; c1:int = 3; d1:int = 4; e1:int = 5;
        a1 = b1 + c1; b1 = d1 + e1; d1 = b1 + c1

        a2:int = 1; b2:int = 2; c2:int = 3; d2:int = 4; e2:int = 5;
        a2 = b2 + c2; b2 = d2 + e2; d2 = b2 + c2

        a3:int = 1; b3:int = 2; c3:int = 3; d3:int = 4; e3:int = 5;
        a3 = b3 + c3; b3 = d3 + e3; d3 = b3 + c3

        a4:int = 1; b4:int = 2; c4:int = 3; d4:int = 4; e4:int = 5;
        a4 = b4 + c4; b4 = d4 + e4; d4 = b4 + c4

        i = i + 1
    }
}
