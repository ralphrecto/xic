
main (args : int[][]) {
  println(string_of_int (ifTest14 ()));
}

// dangling else
ifTest14() : int {
  x:int,r:int;
  x = 0;
  if (x < 1) if (2 > x) r = 1; else r = 0;

  return r;
}
