use io use conv
abs(i:int) : int { if (i < 0) { return -i } else { return i } }
g(a:int[][]) : int[] { return a[abs(42 *>> a[0][0]) % length(a)] }
f(a:int[]) : int {
  i:int = 0; j:int = 0;
  while (i < length(a)) {
    a[i] = a[i] *>> a[i/2] + a[0];
    j = j + a[i];
  }
  return abs(j) % length(a)
}
h(a:int[], j:int) : int {
  if (j < length(a) & j >= 0) { return j }
  else { return f(a) }
}
main(args:int[][]) {
  a:int[] = {-9223372036854775808,9223372036854775807,18787,-183014,-24740,12};
  aa:int[][] = {a, {382,-988} + a + {923}, a+a, a+a+a}
  println(unparseInt({a[h(a,f(g(aa)))], a[f(a)], h(g(aa), f(g(aa)))}[0]))
}
