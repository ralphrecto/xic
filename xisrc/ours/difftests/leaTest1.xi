
main(args : int[][]) {
  // lea-case1
  println( "Expected: 3. Actual: " + string_of_int (leaTest1()) );
  println( "Expected: 4. Actual: " + string_of_int (leaTest2()) );
  println( "Expected: 6. Actual: " + string_of_int (leaTest3()) );
  println( "Expected: 10. Actual: " + string_of_int (leaTest4()) );
  println( "Expected: 4. Actual: " + string_of_int (leaTest5()) );
  println( "Expected: 4. Actual: " + string_of_int (leaTest6()) );
  println( "Expected: 4. Actual: " + string_of_int (leaTest7()) );
  println( "Expected: 4. Actual: " + string_of_int (leaTest8()) );
  println( "Expected: 2. Actual: " + string_of_int (leaTest9()) );

  // lea-case2
  // lea-case3
  // lea-case4
  // lea-case5
  // lea-case6
  // lea-case7
}

// lea-case1: c + (r1 * {1,2,4,8} + r2)
leaTest1() : int {
  x : int = 1;
  y : int = 1;
  return 1 + ((y * 1) + x);
}

leaTest2() : int {
  x : int = 1;
  y : int = 1;
  return 1 + ((y * 2) + x);
}

leaTest3() : int {
  x : int = 1;
  y : int = 1;
  return 1 + ((y * 4) + x);
}

leaTest4() : int {
  x : int = 1;
  y : int = 1;
  return 1 + ((y * 8) + x);
}

leaTest5() : int {
  x : int = 1;
  y : int = 1;
  return 1 + ((2 * y) + x);
}

// lea-case1: c + (r2 + r1 * {1,2,4,8})
leaTest6() : int {
  x : int = 1;
  y : int = 1;
  return 1 + (x + y * 2);
}

leaTest7() : int {
  x : int = 1;
  y : int = 1;
  return 1 + (x + 2 * y);
}

// lea-case1: (x * {1,2,4,8} + y) +/- c
leaTest8() : int {
  x : int = 1;
  y : int = 1;
  return (x * 2 + y) + 1;
}

leaTest9() : int {
  x : int = 1;
  y : int = 1;
  return (x * 2 + y) - 1;
}

leaTest10() : int {
  x : int = 1;
  y : int = 1;
  return (2 * x + y) + 1;
} 

leaTest11() : int {
  x:int = 1;
  y:int = 1;
  return (2 * x + y) - 1;
}

// lea-case1: (r2 + r1 * {1,2,4,8}) +/- c
leaTest12() : int {
  x:int = 1;
  y:int = 1;
  return (x + y * 2) + 1;
}

leaTest13() : int {
  x:int = 1;
  y:int = 1;
  return (x + y * 2) - 1;
}

leaTest14() : int {
  x:int = 1;
  y:int = 1;
  return (x + 2 * y) + 1;
}

leaTest15() : int {
  x:int = 1;
  y:int = 1;
  return (x + 2 * y) - 1;
}

// lea-case1: r2 + (r1 * {1,2,4,8} +/- c)
leaTest16() : int {
  x:int = 1;
  y:int = 1;
  return y + (x * 1 + 1);
}

leaTest17() : int {
  x:int = 1;
  y:int = 1;
  return y + (1 * x + 1);
}

// lea-case1: r2 + (c + r1 * {1,2,4,8})
leaTest18() : int {
  x:int = 1;
  y:int = 1;
  return y + (1 + x * 1);
}
// lea-case1: (r1 * {1,2,4,8} +/- c) + r2
// lea-case1: (c + r1 * {1,2,4,8}) + r2


