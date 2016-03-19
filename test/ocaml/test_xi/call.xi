main () {
    {
      print ("Expecting false. Actual: " + string_of_bool (foo()));
      print (string_of_int (eq (17)));
      print (string_of_int (mod (13110, 10000)));
    }
   
    {
      // Testing multiple return
      x:int, y:int, z:int = square(1, 2, 3); 
      print (string_of_int x);
      print (string_of_int y);
      print (string_of_int z);
    }

    {
      // Testing weird array cases
      a:int[] = {10, 20, 30};
      b:int[], c:int[] = arr2(a, a);
      print ("Expecting 40. Actual: " + string_of_int (a[2]));
      print ("Expecting true. Actual: " + string_of_bool (arr_eq (b, c)));

      d:int[] = {17, 18, 19};
      e:int[3];
      e = d;
      arr1(d);
      print ("Expecting true. Actual: " + string_of_bool (e[0] == 42));
    }
}

foo () : bool {
    return false;
}

eq (x:int) : bool {
    return x == x;
}

mod (x:int, y:int) : int {
    return x % y;
}

square (x:int, y:int, z:int) : int, int, int {
    return (x * x, y * y, z * z);
}

arr1 (a:int[]) {
    a[0] = 42;
}

arr2 (a1:int[], a2:int[]) : int[], int[] {
    a1[2] = 40;
    return a1, a1;
}

