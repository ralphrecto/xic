f1(x1:int, x2:int, x3:int, x4:int, x5:int, x6:int, x7:int, x8:int) : int {
  return x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8;
}

f2() : int, int, int, int, int, int, int, int {
  return 1, 2, 3, 4, 5, 6, 7, 8;
}

f3() {
  x1:int, x2:int, x3:int, x4:int, x5:int, x6:int, x7:int, x8:int = f2();
  println(unparseInt(f1(x1, x2, x3, x4, x5, x6, x7, x8)));
}

f4(x1:int, x2:int, x3:int, x4:int, x5:int, x6:int, x7:int, x8:int) : int, int, int, int, int, int, int, int {
  return x1, x2, x3, x4, x5, x6, x7, x8;
}

main(_:int[][]) {
  f3();
  x1:int, x2:int, x3:int, x4:int, x5:int, x6:int, x7:int, x8:int = f4(1,2,3,4,5,6,7,8);
  println(unparseInt(f1(x1, x2, x3, x4, x5, x6, x7, x8)));
}

