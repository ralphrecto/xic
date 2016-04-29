whileTest1() : int {
  x:int = 0;
  z:int = 0;

  while (x < 5) {
    z = z - 1;
    x = x + 1;
  }

  return z;
}

whileTest2() : int {
  x:int = 0;
  y:int = 0;
  z:int = 0;

  while (y < 5) {
    while (x < 10) {
      z = z + 1;
      x = x + 1;
    }
    y = y + 1;
  }

  return z;
}

whileTest3() : int {
  x:bool, y:bool;
  x = true;
  y = true;
  z:int = 0;

  while (x) {
    z = z + 1;
    while (y) {
      z = z + 2;
      y = false;
    }
    x = false;
  }

  return z;
}

whileTest4() : int {
  x:bool, y:bool;
  x = 4 == 1;
  y = true == false;
  z: int = 1;

  while (x) {
    while (y) {
      z = z + 1;
    }
  }

  return z;
}   

whileTest5() : int {
  w:int, x:int, y:int, z:int;
  w = 0; x = 0; y = 0; z = 0;

  while (x < 1) {
    while (y < 1) {
      while (z < 1) {
        w = w + 1;
        z = z + 1;
      }
      w = w + 2;
      y = y + 1;
    }
    w = w + 3;
    x = x + 1;
  }

  return w;
}

main (_:int[][]) {
  println (string_of_int (whileTest1()));
  println (string_of_int (whileTest2()));
  println (string_of_int (whileTest3()));
  println (string_of_int (whileTest4()));
  println (string_of_int (whileTest5()));
}

