breakTest1() {
    while (true) {
        break
    }
    println("1")
}

breakTest2() {
    while (true) {
        while (true) {
            break
        }
        break
    }
    println("2")
}

breakTest3() {
    while (true) {
        while (false) {
        }
        break
    }
    println("3")
}

breakTest4() {
    i: int = 0
    while (i < 10) {
        println("4")
        i = i + 1
        if (i == 5) {
            break
        }
    }
}

breakTest5() {
    i: int = 0
    while (i < 10) {
        println("5")
        j: int = 0
        while (j < 10) {
            if (i == 5 | j == 2) {
                while (false) {}
                break
            }
            j = j + 1
        }
        if (i == 5) {
            break
        }
        i = i + 1
    }
}

main(_: int[][]) {
    breakTest1()
    breakTest2()
    breakTest3()
    breakTest4()
    breakTest5()
}
