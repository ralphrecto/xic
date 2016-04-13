
main (args : int[][]) {
  println(string_of_int (ifTest10 ()));
}

ifTest10() : int {
  x:int, r:int;
  x = 5;
  if ( x > 0 ) r = 1;
  else r = 0;

  return r;
}
