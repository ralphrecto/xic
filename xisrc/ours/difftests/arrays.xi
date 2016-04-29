test1() {
    x: int[]
}

test2() {
    x: int[1]
}

test3() {
    x: int[10]
}

test4() {
    x: int[10]
    x[0] = 1
}

test5() {
    x: int[10]
    x[0] = 1
    x[1] = 2
    x[2] = 3
    x[3] = 4
    x[4] = 5
    x[5] = 6
    x[6] = 7
    x[7] = 8
    x[8] = 9
    x[9] = 10
}

test6() {
    x: int[10]
    x[0] = 1
    x[1] = 2
    x[2] = 3
    x[3] = 4
    x[4] = 5
    x[5] = 6
    x[6] = 7
    x[7] = 8
    x[8] = 9
    x[9] = 10
    print_1array(x)
}

test7() {
    x: int[] = {}
    print_1array(x)
}

test8() {
    x: int[] = {1}
    print_1array(x)
}

test9() {
    x: int[] = {1, 2, 3, 4, 5, 6}
    print_1array(x)
}

test10() {
    x: int[] = {1, 2, 3, 4, 5, 6}
    x[0] = 10
    x[1] = 20
    x[2] = 30
    x[3] = 40
    x[4] = 50
    x[5] = 60
    print_1array(x)
}

foo11(x: int[]) { }
test11() {
    x: int[] = {
        1,   2,   3,   4,   5,   6,   7,   8,   9,
        11,  12,  13,  14,  15,  16,  17,  18,  19,
        21,  22,  23,  24,  25,  26,  27,  28,  29,
    }
    print_1array(x)
    foo11(x)
    print_1array(x)
}

foo12(x: int[]) {
    x[0] = 1
}
test12() {
    x: int[] = {1, 2, 3, 4, 5, 6}
    foo12(x)
    print_1array(x)
}

test13() {
    x:int[] = {1, 2, 3}
    y:int[] = {4, 5, 6}
    z:int[] = x + y
    z = z + z
    print_1array(x)
    print_1array(y)
    print_1array(z)
}

double(x: int[]) : int[] {
    return  x + x
}
test14() {
    s: int[] = {1}
    s = "{{{{" + s
}

mangle_first(x: int[]) {
    x[0] = 42
}
test15() {
    x: int[] = {0, 1, 2, 3}
    print_1array(x)
    mangle_first(x)
    print_1array(x)
}

main(_: int[][]) {
    test1()
    test2()
    test3()
    test4()
    test5()
    test6()
    test7()
    test8()
    test9()
    test10()
    test11()
    test12()
    test13()
    test14()
    test15()
}
