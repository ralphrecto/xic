main (args : int[][]) {
    // Testing binops
    print (string_of_int (1 + 7));
    print (string_of_int ('a' + 2));
    print (string_of_int (2 + 'a'));
    print (string_of_int ('a' + 'b'));

    // Testing unops
    print (string_of_int (-5));
    print (string_of_int (-({1, 2, 3}[0])));
    print (string_of_bool (!true));
    print (string_of_bool (!(true&false)));
    print (string_of_bool (!({true, false, true}[2])));
}
