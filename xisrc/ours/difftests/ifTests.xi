ifTest1() : int {
  r:int;
  if (true) r = 1
  else r = 0

  return r;
}

ifTest2() : int {
  r:int;
  if (true) r = 1

  return r;
}

ifTest3() : int {
  if (true) { return 1; }
  else { return 0; }
}

ifTest4() : int {
  if (true) { return 1; }
  return 0;
}

ifTest5() : int {
  if (1 == (0 + 2)) { return 0; }
  else { return 1; }
}

ifTest6() : int {
  if (1 > 0) { return 1; }
  else { return 0; }
}

ifTest7() : int {
  if (0 <= 1) { return 1; }
  return 0;
}

ifTest8() : int {
  x:int;
  r:int;
  x = 5;
  if (x < 7) r = 1;
  else r = 0;

  return r;
}

ifTest9() : int {
  x:int; y:int; r:int;
  x = 5;
  y = x + 5;
  if (y > x) r = 1;
  else r = 0;

  return r;
}

ifTest10() : int {
  x:int; r:int;
  x = 5;
  if ( x > 0 ) r = 1;
  else r = 0;

  return r;
}

ifTest11() : int {
  x:int; r:int;
  x = 0;
  r = 0;
  if (x >= 0) {
    while (x < 10) {
      x = x + 1;
      r = r + 1;
    }
  }

  r = r - 9;
  return r;
}

ifTest12() : int {
  x:int; r:int;
  x = 9;
  r = 0;
  while ( 0 <= x ) {
    if ( 0 >= x ) r = r + 1;
    x = x - 1;
  }

  r = r - 9
  return r;
}

// nested if statements
ifTest13() : int {
  x:int; y:int; r:int;
  x = 0;
  y = 0;
  r = 0;
  if ( x == 0 ) {
    if ( y == 0 ) {
        r = r + 1;
    }
    r = r + 10;
  }

  r = r - 10;
  return r;
}

// dangling else
ifTest14() : int {
  x:int;r:int;
  x = 0;
  if (x < 1) if (2 > x) r = 1; else r = 0;

  return r;
}

main(_: int[][]) {
  println (string_of_int (ifTest1 ()));
  println (string_of_int (ifTest2 ()));
  println (string_of_int (ifTest3 ()));
  println (string_of_int (ifTest4 ()));
  println (string_of_int (ifTest5 ()));
  println (string_of_int (ifTest6 ()));
  println (string_of_int (ifTest7 ()));
  println (string_of_int (ifTest8 ()));
  println (string_of_int (ifTest9 ()));
  println (string_of_int (ifTest10 ()));
  println (string_of_int (ifTest11 ()));
  println (string_of_int (ifTest12 ()));
  println (string_of_int (ifTest13 ()));
  println (string_of_int (ifTest14 ()));
}
