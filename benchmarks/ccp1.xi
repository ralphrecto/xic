main(args: int[][]) {
    x: int = 2
    y: int = x * x + x
    if (y > 10) {
        x0:int=0; x1:int=1; x2:int=2; x3:int=3; x4:int=4; x5:int=5; x6:int=6;
        x7:int=7; x8:int=8; x9:int=9; x10:int=10; x11:int=11; x12:int=12;
        x13:int=13; x14:int=14; x15:int=15; x16:int=16; x17:int=17; x18:int=18;
        x19:int=19; x20:int=20; x21:int=21; x22:int=22; x23:int=23; x24:int=24;
        x25:int=25; x26:int=26; x27:int=27; x28:int=28; x29:int=29; x30:int=30;
        y = x0 + x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 +
        x13 + x14 + x15 + x16 + x17 + x18 + x19 + x20 + x21 + x22 + x23 + x24 +
        x25 + x26 + x27 + x28 + x29 + x30
    }
    y0:int=0; y1:int=1; y2:int=2; y3:int=3; y4:int=4; y5:int=5
    while (x < 100 * 1000 * 1000) {
        y = y0 + y1 + y2 + y3 + y4 + y5
        x = x + 1
    }
}
