
main(args : int[][]) {
  println (string_of_int (ifTest1 ()));
}

ifTest1() : int {
  r:int;
  if (true) r = 1
  else r = 0

  return r;
}
