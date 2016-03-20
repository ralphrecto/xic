main (args : int[][]) {
    {
      // Testing weird array cases
      a:int[] = {10, 20, 30};
      b:int[], c:int[] = arr2(a, a);
      println ("Expecting 40. Actual: " + string_of_int (a[2]));
      println ("Expecting true. Actual: " + string_of_bool (arr_eq (b, c)));

      d:int[] = {17, 18, 19};
      e:int[3];
      e = d;
      arr1(d);
      println ("Expecting true. Actual: " + string_of_bool (e[0] == 42));
    }
}

arr1 (a:int[]) {
    a[0] = 42;
}

arr2 (a1:int[], a2:int[]) : int[], int[] {
    a1[2] = 40;
    return a1, a1;
}

