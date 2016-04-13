wheel1(x1:int[], x2:bool) : int, int {
  if (x2) return 1, 2;
  y:int;
  if (x2) then {
    y = 0;
  } else {
    y = 1;
  }
  return wheel2(x1, y);
}

wheel2(x1:int[], x2:int) : int, int {
  y:int;
  if (length(x1) > 0) {
    y = x1[0] + x2; 
  } else {
    y = x2;
  }
  return wheel3(y, y == x2);
}

wheel3(x1:int, _:bool) : int, int {
  return wheel1({x1, x1+1}, true);
}

main(_:int[][]) {
  x:int, y:int = wheel1({1}, false);
  println(unparseInt(x));
  println(unparseInt(y));
}
