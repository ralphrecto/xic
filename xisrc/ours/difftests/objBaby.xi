use io
use conv

class Matrix {
  m:int[][]
  // returns row, col
  getDim() : int, int {
    return length(this.m), length(this.m[0]);
  }
}

newMatrix(row:int, col:int) : Matrix {
  m:Matrix = new Matrix
  backing:int[row][col];
  m.m = backing;
  return m;
}

main(_:int[][]) {
  m: Matrix = newMatrix(10, 10);
  row: int, col: int = m.getDim();
  println("row: " + unparseInt(row) + ", " + "col : " + unparseInt(col));
}
