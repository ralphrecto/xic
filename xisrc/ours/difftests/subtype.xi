class A {}
class B extends A {}

// assignment
test1() {
    x: int[] = null
    x = null
    x = {}
    y: A = new A
    z: A = new B
    a: B = new B
    i: A = null
    i = new B
    println("a")
}

// arrays
test2() {
    { x:int[]   = null         print_1array(x) }
    { x:int[]   = {}           print_1array(x) }
    { x:int[][] = null         print_2array(x) }
    { x:int[][] = {null}       print_2array(x) }
    { x:int[][] = {{}}         print_2array(x) }
    { x:int[][] = {{}, null}   print_2array(x) }
    { x:int[][] = {null, {}}   print_2array(x) }
    { x:int[][] = {{}, {}}     print_2array(x) }
    { x:int[][] = {null, null} print_2array(x) }

    { x:A[] = null }
    { x:A[] = {} }
    { x:A[] = {null} }
    { x:A[] = {null, null} }
    { x:A[] = {new A, new B} }
    { x:A[] = {new B, new A} }
    { x:A[] = {new A} }
    { x:B[] = {new B} }

    { x:int[][][] = {{}, {null}, null, null, {null,{}}}
      print_3array(x) }
}

// arguments
test3_helper(x: int[], y: A) : int[], A[] {
    return null, {}
}

test3() {
    {
        xs: int[], as: A[] = test3_helper(null, null)
        print_1array(xs)
    }
    {
        xs: int[], as: A[] = test3_helper({}, new B)
        print_1array(xs)
    }
}

// length
test4() {
    xs: int[] = {}
    println(string_of_int(length(xs)))
    println(string_of_int(length({})))
}

// index
test5() {
    xs: A[] = {new A, new B, new A}
    xs[0] = new A
    xs[1] = new B
    xs[1] = null
    x: A = xs[0]
}

// return
foo1() : A[] { return null }
foo2() : A[] { return {} }
foo3() : A[] { return {new A} }
foo4() : A[] { return {new A, new B} }
test6() {
    _ = foo1()
    _ = foo2()
    _ = foo3()
    _ = foo4()
}

main(_: int[][]) {
    test1()
    test2()
    test3()
    test4()
    test5()
    test6()
}
