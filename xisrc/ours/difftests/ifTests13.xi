
main (args : int[][]) {
  println(string_of_int (ifTest13 ()));
}

// nested if statements
ifTest13() : int {
  x:int, y:int, r:int;
  x = 0;
  y = 0;
  r = 0;
  if ( x == 0 ) {
    if ( y == 0 ) {
        r = r + 1;
    }
    r = r + 10;
  }

  r = r - 10;
  return r;
}
