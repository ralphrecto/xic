
main (args : int[][]) {
  println(string_of_int (ifTest9 ()));
}

ifTest9() : int {
  x:int, y:int, r:int;
  x = 5;
  y = x + 5;
  if (y > x) r = 1;
  else r = 0;

  return r;
}
