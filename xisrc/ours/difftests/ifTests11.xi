
main (args : int[][]) {
  println(string_of_int (ifTest11 ()));
}

ifTest11() : int {
  x:int, r:int;
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
