main(_: int[][]) {
    println(string_of_int(2 + 3));
    println(string_of_int(2 - 3));
    println(string_of_int(2 * 3));
    println(string_of_int(2 *>> 3));
    println(string_of_int(2 / 3));
    println(string_of_int(2 % 3));
    println(string_of_bool(true & false));
    println(string_of_bool(false | true));
    println(string_of_bool(2 == 3));
    println(string_of_bool(2 != 3));
    println(string_of_bool(true == false));
    println(string_of_bool(true != false));
    println(string_of_bool({} == {}));
    println(string_of_bool({} != {}));
    println(string_of_bool(2 < 3));
    println(string_of_bool(2 > 3));
    println(string_of_bool(2 >= 3));
    println(string_of_bool(2 <= 3));

    println(string_of_int(1 % 0));
    println(string_of_int(2 % 0));
    println(string_of_int(1 / 0));
    println(string_of_int(2 / 0));
}
