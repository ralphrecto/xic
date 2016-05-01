main (_:int[][]){
  x:int = 0
  y:int[] = {1, 2, 3}
  z:int = 5
  v:int
  while (x < z) {
    x = x + 1
    v = length(y)
  }
  println(unparseInt(x))
  println(unparseInt(v))
}
