g1:int = 1
g2:int
g3:int = g2
g4:int = g2 + 3
g5:int = g6
g6:int = g7 + 5
g7:int

globalTest1() : int {
  return g1;
}

globalTest2() : int {
  g1 = 16;
  return g1;
}

main (_: int[][]) {
  println (string_of_int (globalTest1()));
  println (string_of_int (globalTest2()));
  println (string_of_int (g2));
  println (string_of_int (g3));
  println (string_of_int (g4));
  println (string_of_int (g5));
  println (string_of_int (g6));
  println (string_of_int (g7));
}
