use io use conv
abs(i:int) : int { if (i < 0) { return -i } else { return i } }
f(a:int[]) : int {
  i:int = 0; j:int = 0;
  while (i < length(a)) {
    a[i] = a[i] * a[i/2] + a[0];
    j = j + a[i];
    i = i + 1
  }
  return abs(j) % length(a)
}
main(args:int[][]) {
  a:int[] = {1,2,3};
  println(unparseInt(a[f(a)]))
}
