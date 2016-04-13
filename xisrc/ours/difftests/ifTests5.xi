
main(args : int[][]) {
  println (string_of_int (ifTest5 ())); 
}

ifTest5() : int {
  if (1 == (0 + 2)) { return 0; }
  else { return 1; }
}
