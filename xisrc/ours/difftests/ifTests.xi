
main(args : int[][]) {
  println (string_of_int (ifTest1 ()));
  println (string_of_int (ifTest2 ()));
  println (string_of_int (ifTest3 ())); 
  println (string_of_int (ifTest4 ())); 
  println (string_of_int (ifTest5 ())); 
  println (string_of_int (ifTest6 ())); 
  println (string_of_int (ifTest7 ())); 
}

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
