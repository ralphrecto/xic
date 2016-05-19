class A {
  x:int
  f(y:int) : int {
    return x + y
  }
}

testFunc(a:A) : int {
  return a.f(1)
}

objTest1() {
  a:A = new A;
  a.x = 0;
  println(unparseInt(testFunc(a)));
}

main(_:int[][]) {
  objTest1();
}
