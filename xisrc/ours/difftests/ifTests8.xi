
main (args : int[][]) {
  println(string_of_int (ifTest8 ()));
}

ifTest8() : int {
  x:int, r:int;
  x = 5;
  if (x < 7) r = 1;
  else r = 0;

  return r;
}
