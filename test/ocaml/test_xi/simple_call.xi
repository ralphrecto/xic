main (args : int[][]) {
    {
      println ("Expecting false. Actual: " + string_of_bool (foo()));
      println (string_of_bool (eq (17)));
      println (string_of_int (mod (13110, 10000)));
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
