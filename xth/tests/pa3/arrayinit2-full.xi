foo() {
    n: int = gcd(10, 2)
    a: int[n]
    while (n > 0) {
      n = n - 1
      a[n] = n
    }
}

// Return the greatest common divisor of two integers
gcd(a:int, b:int):int {
  while (a != 0) {
    if (a<b) b = b - a
    else a = a - b
  }
  return b
}
