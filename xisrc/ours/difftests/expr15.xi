word(x:int[]){
    z:int = 0;
    w:int[] = "{";
    while (z < length(x)) {
        t: int[] = unparseInt(x[z]); 
        w = w + t + ",";
        z = z+1;
    }
    w = w + "}";
    println(w);
}

what(x:int[][]){
  y:int = 0;
  println ("{");
  while (y < length(x)) {
    word(x[y]);
    y = y+1;
  } 
  println("}");
}

main(args:int[][]) {
  x:int[][] = {{1},{1},{1}};
  what(x);
}

