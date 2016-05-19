// Basic globals and ordering
g1:int = 1
g2:int
g3:int = g2
g4:int = g2 + 3
g5:int = g6
g6:int = g7 + 5
g7:int

// Arrays
g8:int[]
g9:int[] = g10
g10:int[] = {}
g11:int[] = g12 + {10, 11, 12}
g12:int[] = {1, 2, 3} + g13
g13:int[] = {4, 5, 6}
g14:int[] = g11 + g12
g15:int[][] = {} + {g13 + g13} + {g12}
g16:int[][] = {}
g17:int[][] = {} + {}
g18:int = g15[0][0]
g19:int[] = g15[1]
g20:int = {{1,2,3},{10,20},{100}}[2][0]

globalTest1() : int {
  return g1;
}

globalTest2() : int {
  g1 = 16;
  return g1;
}

main (_: int[][]) {
  println (string_of_int (globalTest1()));
  println (string_of_int (globalTest2()));
  println (string_of_int (g2));
  println (string_of_int (g3));
  println (string_of_int (g4));
  println (string_of_int (g5));
  println (string_of_int (g6));
  println (string_of_int (g7));

  print_1array (g8);
  print_1array (g9);
  print_1array (g10);
  print_1array (g11);
  print_1array (g12);
  print_1array (g13);
  print_1array (g14);
  print_2array (g15);
  print_2array (g16);
  print_2array (g17);
  println (string_of_int (g18));
  print_1array (g19);
  println (string_of_int (g20));
}
