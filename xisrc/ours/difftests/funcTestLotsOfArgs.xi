foo(
    x1:int,  x2:int,  x3:int,  x4:int,  x5:int,
    x6:int,  x7:int,  x8:int,  x9:int,  x10:int,
    x11:int, x12:int, x13:int, x14:int, x15:int,
    x16:int, x17:int, x18:int, x19:int, x20:int
) : int {
    return
        x1  + x2  + x3  + x4  + x5  +
        x6  + x7  + x8  + x9  + x10 +
        x11 + x12 + x13 + x14 + x15 +
        x16 + x17 + x18 + x19 + x20
}

bar() : int {
    return 42
}

main(_: int[][]) {
    println(unparseInt(bar()))
    println(unparseInt(foo(
        10,  20,  30,  40,  50,
        60,  70,  80,  90,  100,
        110, 120, 130, 140, 150,
        160, 170, 180, 190, 200
    )))
    println(unparseInt(bar()))
    println(unparseInt(bar()))
}
