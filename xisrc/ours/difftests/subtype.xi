class A {}
class B extends A {}

// assignment
test1() {
    x: int[] = null
    y: A = new A
    z: A = new B
    a: B = new B
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
    xs: int[], as: A[] = test3_helper(null, null)
}

main(_: int[][]) {
    test1()
    test2()
    test3()
}
