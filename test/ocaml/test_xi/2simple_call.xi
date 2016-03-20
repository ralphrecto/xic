main (args : int[][]) {
    {
      // Testing multiple return
      x:int, y:int, z:int = square(1, 2, 3); 
      println (string_of_int(x));
      println (string_of_int(y));
      println (string_of_int(z));
    }
}

square (x:int, y:int, z:int) : int, int, int {
    return x * x, y * y, z * z;
}
