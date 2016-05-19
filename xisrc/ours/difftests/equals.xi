equalsTest1() {
    x: int = 1
    y: int = 1
    check(x == y)
}

equalsTest2() {
    x: bool = true
    y: bool = true
    check(x == y)
}

class A {}
class B extends A {}
class C extends A {}
class D extends B {}
class E {}

equalsTest3() {
    {
        a: A = new A
        b: B = new B
        c: C = new C
        d: D = new D

        check(a == a)
        check(b == b)
        check(c == c)
        check(d == d)

        check(a != b)
        check(a != c)
        check(a != d)
        check(b != d)
    }

    {
        a: A = new D
        b: B = new D
        c: C = new C
        d: D = new D

        check(a == a)
        check(b == b)
        check(c == c)
        check(d == d)

        check(a != b)
        check(a != c)
        check(a != d)
        check(b != d)
    }
}

main(_: int[][]) {
    equalsTest1()
    equalsTest2()
    equalsTest3()
}
