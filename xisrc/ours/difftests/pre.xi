// one redundant expression, no branches
test1() {
    x:int = 1 + 1
    y:int = 1 + 1
    z:int = 1 + 1
    println(string_of_int(x))
    println(string_of_int(y))
    println(string_of_int(z))
}

// multiple redundant expression, no branches
test2() {
    a:int = (1 * 1) + (2 * 2)
    b:int = (1 * 1) + (2 * 2)
    c:int = 1 * 1
    d:int = 2 * 2
    e:int = a + c
    f:int = a + c
    g:int = a + c

    println(string_of_int(a))
    println(string_of_int(b))
    println(string_of_int(c))
    println(string_of_int(d))
    println(string_of_int(e))
    println(string_of_int(f))
    println(string_of_int(g))
}

// single fully redundant expression with branches
test3() {
    x:int = 0
    y:int = 20
    z:int = 30

    if (true) {
        x = y + z
    } else {
        x = y + z
    }
    x = y + z

    println(string_of_int(x))
    println(string_of_int(y))
    println(string_of_int(z))
}

// single partially redundant expression with branches
test4() {
    x:int = 0
    y:int = 20
    z:int = 30

    if (true) {
        x = y + z
    } else {
    }
    x = y + z

    println(string_of_int(x))
    println(string_of_int(y))
    println(string_of_int(z))

    test4v2(10, 20, 30)
    test4v3(10, 20, 30)
}

test4v2(x:int, y:int, z:int) {
    if (y != z) {
        x = (y + z) + (y + z)
        println("hi")
    }
    x = y + z

    println(string_of_int(x))
    println(string_of_int(y))
    println(string_of_int(z))
}

test4v3(x:int, y:int, z:int) {
    if (y != z) {
        x = (y + z) + (y + z)
        x = (y + z) + (y + z)
        println("hi")
    }
    x = y + z

    println(string_of_int(x))
    println(string_of_int(y))
    println(string_of_int(z))
}

// complicated
test5() {
    a:int = 10
    b:int = 20
    c:int = 30

    x:int
    y:int
    z:int

    while (a < c) {
        a = a + a
        z = b + b
    }
    z = b + b
    x = a + a
    y = (b + b) + (a + a) + z

    if (x < y) {
        z = x + y
        while (x < y) {
            z = z + (x + y)
            x = x + y
            x = x + y
        }
    } else {
        z = y + x
    }

    println(string_of_int(a))
    println(string_of_int(b))
    println(string_of_int(c))
    println(string_of_int(x))
    println(string_of_int(y))
    println(string_of_int(z))
}

// arrays
test6() {
    x: int[] = {1, 2, 3}
    x[1 + 1] = x[1 + 1] + (1 + 1)
    println(string_of_1array(x))
}

// troll test case
test7() {
    x:int = 0
    num:int = 10
    den:int =  0

    if (false) {
        x = num / den
        x = num % den
    }

    if (true) {
    } else {
        x = num / den
        x = num % den
    }

    while (false) {
        x = num / den
        x = num % den
    }

    println(string_of_int(x))
    println(string_of_int(num))
    println(string_of_int(den))
}

// super complicated
foo(x: int, y: int, z: int[]) : int {
    println("I'll substitute eminem for eminem in the body")
    println("After two steps, that will give me s n m bar")
    println("And then after a bunch of steps...that's gonna give me m + n")
    z[0] = z[0] + 1
    return x + y + z[0]
}

test8() {
    z:int[] = {1, 2, 3}
    x:int = foo(1 * 2, 1 * 2, z)
    y:int = foo(1 * 2, 2 * 1, z)

    if (true) {
        x = foo(z[1 * 2], foo(1, 2, z), z) + foo(1 * 2, 42, z)
    } else {
    }

    if (false) {
    } else {
        y = foo(x + y, x + y, z) + foo(1, 1, z)
    }

    println(string_of_int(x))
    println(string_of_int(y))
    println(string_of_1array(z))
}

test9() {
  a:int = 3;
  if (a < 10) {
    a = a + 1;
  }
  while (a < 10) {
    a = a + 1;
  }
}

test10() {
  a:int = 1;
  b:int = 2;
  x:int = (a+b) + (a+b);
}

main(_: int[][]) {
    test1();
    test2();
    test3();
    test4();
    test5();
    test6();
    test7();
    test8();
    test9();
    test10();
}
