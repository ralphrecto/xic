foo(
    x1:int,  x2:int,  x3:int,  x4:int,  x5:int,
    x6:int,  x7:int,  x8:int,  x9:int,  x10:int,
    x11:int, x12:int, x13:int, x14:int, x15:int,
    x16:int, x17:int, x18:int, x19:int, x20:int
) :
    int, int, int, int, int,
    int, int, int, int, int,
    int, int, int, int, int,
    int, int, int, int, int
{
    return
        x20, x19, x18, x17, x16,
        x15, x14, x13, x12, x11,
        x10, x9,  x8,  x7,  x6,
        x5,  x4,  x3,  x2,  x1
}

bar() : int{
    return 42
}

main(_: int[][]) {
    println(unparseInt(bar()))
    println(unparseInt(bar()))

    x1:int,  x2:int,  x3:int,  x4:int,  x5:int,
    x6:int,  x7:int,  x8:int,  x9:int,  x10:int,
    x11:int, x12:int, x13:int, x14:int, x15:int,
    x16:int, x17:int, x18:int, x19:int, x20:int = foo(
        10,  20,  30,  40,  50,
        60,  70,  80,  90,  100,
        110, 120, 130, 140, 150,
        160, 170, 180, 190, 200
    )
    println(unparseInt(x1))  println(unparseInt(x2))  println(unparseInt(x3))
    println(unparseInt(x4))  println(unparseInt(x5))  println(unparseInt(x6))
    println(unparseInt(x7))  println(unparseInt(x8))  println(unparseInt(x9))
    println(unparseInt(x10)) println(unparseInt(x11)) println(unparseInt(x12))
    println(unparseInt(x13)) println(unparseInt(x14)) println(unparseInt(x15))
    println(unparseInt(x16)) println(unparseInt(x17)) println(unparseInt(x18))
    println(unparseInt(x19)) println(unparseInt(x20))

    y1:int,  y2:int,  y3:int,  y4:int,  y5:int,
    y6:int,  y7:int,  y8:int,  y9:int,  y10:int,
    y11:int, y12:int, y13:int, y14:int, y15:int,
    y16:int, y17:int, y18:int, y19:int, y20:int = foo(
        10,  20,  30,  40,  50,
        60,  70,  80,  90,  100,
        110, 120, 130, 140, 150,
        160, 170, 180, 190, 200
    )
    println(unparseInt(y1))  println(unparseInt(y2))  println(unparseInt(y3))
    println(unparseInt(y4))  println(unparseInt(y5))  println(unparseInt(y6))
    println(unparseInt(y7))  println(unparseInt(y8))  println(unparseInt(y9))
    println(unparseInt(y10)) println(unparseInt(y11)) println(unparseInt(y12))
    println(unparseInt(y13)) println(unparseInt(y14)) println(unparseInt(y15))
    println(unparseInt(y16)) println(unparseInt(y17)) println(unparseInt(y18))
    println(unparseInt(y19)) println(unparseInt(y20))

    println(unparseInt(bar()))
    println(unparseInt(bar()))
}
