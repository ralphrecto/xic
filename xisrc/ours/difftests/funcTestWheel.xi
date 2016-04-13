use conv
use io

wheel1(x1:int[], x2:bool) : int, int {
  if (x2) { return 1, 2; }
  y:int;
  if (x2) {
    y = 0;
  } else {
    y = 1;
  }
  z1:int, z2:int = wheel2(x1, y);
  return z1, z2;
}

wheel2(x1:int[], x2:int) : int, int {
  y:int;
  if (length(x1) > 0) {
    y = x1[0] + x2; 
  } else {
    y = x2;
  }
  z1:int, z2:int = wheel3(y, y == x2);
  return z1, z2;
}

wheel3(x1:int, _:bool) : int, int {
  z1:int, z2:int = wheel1({x1, x1+1}, true);
  return z1, z2;
}

main(_:int[][]) {
  x:int, y:int = wheel1({1}, false);
  println(unparseInt(x));
  println(unparseInt(y));
}
