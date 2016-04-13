main (args : int[][]) {
    // Testing binops
    println (string_of_int (1 + 7));
    println (string_of_int ('a' + 2));
    println (string_of_int (2 + 'a'));
    println (string_of_int ('a' + 'b'));

    // Testing unops
    println (string_of_int (-5));
    println (string_of_int (-({1, 2, 3}[0])));
    println (string_of_bool (!true));
    println (string_of_bool (!(true&false)));
    println (string_of_bool (!({true, false, true}[2])));
}
