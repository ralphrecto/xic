class A {
    foo() {
        println("a")
    }

    bar() {
        println("b")
    }

    baz() {
        bar()
    }
}

class B extends A {
    bar() {
        println("c")
    }
}

test1() {
    {
        a: A = new A
        b: B = new B
        a.foo()
        a.bar()
        a.baz()
        b.foo()
        b.bar()
        b.baz()
    }
    {
        a: A = new A
        b: A = new B
        a.foo()
        a.bar()
        a.baz()
        b.foo()
        b.bar()
        b.baz()
    }
}

main(_: int[][]) {
    test1()
}
