main () {
    print (string_of_bool (foo()));
    print (string_of_int (eq (17)));
    print (string_of_int (mod (13110, 10000)));
   
    // Testing multiple return
    x:int, y:int, z:int = square(1, 2, 3); 
    print (string_of_int x);
    print (string_of_int y);
    print (string_of_int z);

    // Testing weird array cases
    a:int[] = {10, 20, 30};
    b:int[], c:int[] = arr1(a, a);
    print (string_of_int (a[2]));
    print (string_of_bool (arr_eq (b, c)));
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

arr1 (a1:int[], a2:int[]) : int[], int[] {
    a1[2] = 40;
    return a1, a1;
}
