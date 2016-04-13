
main (args : int[][]) {
  println(string_of_int (ifTest12 ()));
}

ifTest12() : int {
  x:int, r:int;
  x = 9;
  r = 0;
  while ( 0 <= x ) {
    if ( 0 >= x ) r = r + 1;
    x = x - 1;
  }

  r = r - 9
  return r;
}
