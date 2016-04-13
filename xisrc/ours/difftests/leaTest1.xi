
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
  println( "Expected: 4. Actual: " + string_of_int (leaTest10()) );
  println( "Expected: 2. Actual: " + string_of_int (leaTest11()) );
  println( "Expected: 4. Actual: " + string_of_int (leaTest12()) );
  println( "Expected: 2. Actual: " + string_of_int (leaTest13()) );
  println( "Expected: 4. Actual: " + string_of_int (leaTest14()) );
  println( "Expected: 2. Actual: " + string_of_int (leaTest15()) );
  println( string_of_int (leaTest16()) );
  println( string_of_int (leaTest17()) );
  println( string_of_int (leaTest18()) );
  println( string_of_int (leaTest19()) );
  println( string_of_int (leaTest20()) );
  println( string_of_int (leaTest21()) );
  println( string_of_int (leaTest22()) );
  println( string_of_int (leaTest23()) );
  println( string_of_int (leaTest24()) );
  println( string_of_int (leaTest25()) );
  println( string_of_int (leaTest26()) );
  println( string_of_int (leaTest27()) );

  // lea-case2
  println( string_of_int (leaTest28()) );
  println( string_of_int (leaTest29()) );
  println( string_of_int (leaTest30()) );
  println( string_of_int (leaTest31()) );
  println( string_of_int (leaTest32()) );
  println( string_of_int (leaTest33()) );

  // lea-case3
  println( string_of_int (leaTest34()) );
  println( string_of_int (leaTest35()) );
  println( string_of_int (leaTest36()) );
  println( string_of_int (leaTest37()) );
  println( string_of_int (leaTest38()) );
  println( string_of_int (leaTest39()) );
  println( string_of_int (leaTest40()) );
  println( string_of_int (leaTest41()) );
  println( string_of_int (leaTest42()) );
  println( string_of_int (leaTest43()) );
  println( string_of_int (leaTest44()) );
  println( string_of_int (leaTest45()) );
  println( string_of_int (leaTest46()) );
  println( string_of_int (leaTest47()) );
  println( string_of_int (leaTest48()) );

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
  return y + (x * 2 + 1);
}

leaTest17() : int {
  x:int = 1;
  y:int = 1;
  return y + (2 * x + 1);
}

leaTest18() : int {
  x:int = 1;
  y:int = 1;
  return y + (x * 2 - 1);
}

leaTest19() : int {
  x:int = 1;
  y:int = 1;
  return y + (2 * x - 1);
}

// lea-case1: r2 + (c + r1 * {1,2,4,8})
leaTest20() : int {
  x:int = 1;
  y:int = 1;
  return y + (1 + x * 2);
}

leaTest21() : int {
  x:int = 1;
  y:int = 1;
  return y + (1 + 2 * x);
}

// lea-case1: (r1 * {1,2,4,8} +/- c) + r2
leaTest22() : int {
  x:int = 1;
  y:int = 1;
  return (x * 2 + 1) + y;
}

leaTest23() : int {
  x:int = 1;
  y:int = 1;
  return (2 * x + 1) + y;
}

leaTest24() : int {
  x:int = 1;
  y:int = 1;
  return (x * 2 - 1) + y;
}

leaTest25() : int {
  x:int = 1;
  y:int = 1;
  return (2 * x - 1) + y;
}

// lea-case1: (c + r1 * {1,2,4,8}) + r2
leaTest26() : int {
  x:int = 1;
  y:int = 1;
  return (1 + x * 2) + y;
}

leaTest27() : int {
  x:int = 1;
  y:int = 1;
  return (1 + 2 * x) + y;
}

// lea-case2: reg * {1, 2, 3, 4, 5, 8, 9} +/- const
leaTest28() : int {
  x:int = 1;
  return x * 3 + 1;
}

leaTest29() : int {
  x:int = 1;
  return 3 * x + 1;
}

leaTest30() : int {
  x:int = 1;
  return 1 + x * 5;
}

leaTest31() : int {
  x:int = 1;
  return 1 + 5 * x;
}

leaTest32() : int {
  x:int = 1;
  return x * 3 - 1;
}

leaTest33() : int {
  x:int = 1;
  return 3 * x - 1;
}

// lea-case3: reg1 + reg2 +/- const
leaTest34() : int {
  x:int = 1;
  y:int = 1;
  return 7 + x + y;
}

leaTest35() : int {
  x:int = 1;
  y:int = 1;
  return x + y + 7;
}

leaTest36() : int {
  x:int = 1;
  y:int = 1;
  return x + y - 7;
}

leaTest37() : int {
  x:int = 1;
  y:int = 1;
  return x + 7 + y;
}

leaTest38() : int {
  x:int = 1;
  y:int = 1;
  return x - 7 + y;
}

leaTest39() : int {
  x:int = 1;
  y:int = 1;
  return 7 + (x + y);
}

leaTest40() : int {
  x:int = 1;
  y:int = 1;
  return x + (y + 7);
}

leaTest41() : int {
  x:int = 1;
  y:int = 1;
  return x + (y - 7);
}

leaTest42() : int {
  x:int = 1;
  y:int = 1;
  return x + (7 + y);
}

leaTest43() : int {
  x:int = 1;
  return x - 1;
}

helper1() : int {
  return 5;
}

leaTest44() : int {
  return helper1() - 1;
}

leaTest45() : int {
  x:int = 1;
  return x + 1;
}

leaTest46() : int {
  x:int = 1;
  return 1 + x;
}

leaTest47() : int {
  return helper1() + 1;
}

leaTest48() : int {
  return 1 + helper1();
}
