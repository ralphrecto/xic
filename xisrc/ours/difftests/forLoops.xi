use io
use conv

range(low: int, high: int) : int[] {
    xs: int[] = {};
    while (low < high) {
        xs = xs + {low};
        low = low + 1;
    }
    return xs;
}

class IntFold {
  call(acc:int, el:int) : int {
    return acc;
  }
}

class Add {
  call(acc:int, el:int) : int {
    return acc + el;
  }
}

class Mult {
  call(acc:int, el:int) : int {
    return acc * el;
  }
}

class Matrix {
  m:int[][]
  fold(foldf:Add, acc:int) : int {
    curacc:int = acc;
    for (row:int[] in m) {
      for (col:int in row) {
        curacc = foldf.call(curacc, col);
      }
    }
    return curacc;
  }
  fill(k:int) {
    for (i:int in range(0, length(m))) {
      for (j:int in range(0, length(m[i]))) {
        m[i][j] = k;
      }
    }
  }
}

newMatrix(row:int, col:int) : Matrix {
  m:Matrix = new Matrix
  a:int[row][col];
  m.m = a;
  return m;
}

forTest1() {
  for (x:int in {1,2,3,4,5}) {
    println(unparseInt(x));
  }
}

matrixTest() {
  row:int = 10;
  col:int = 10;
  m:Matrix = newMatrix(row, col);
  m.fill(1);
  println(unparseInt(row * col));
  println(unparseInt(m.fold(new Add, 0)));
}

main(_:int[][]) {
  forTest1();
  matrixTest();
}
