empty_first(xs: int[][]) {
    xs[0] = {};
}

main() {
    xs: int[][] = {{}, {1}, {1,2}};
    println(string_of_2array(xs));

    xs[2] = xs[0];
    println(string_of_2array(xs));

    xs[1] = xs[1] + {xs[1][1]};
    println(string_of_2array(xs));

    xs[0] = {} + {} + "a" + "" + {} + {}
    println(string_of_2array(xs));

    empty_first(xs);
    println(string_of_2array(xs));

    xs[0] = {} + {0} + {};
    println(string_of_2array(xs));

    xs[0] = xs[0] + xs[0] + {xs[xs[0][0]][xs[0][0]]}
    println(string_of_2array(xs));

    xs[2] = xs[2] + xs[0] + xs[1]
    println(string_of_2array(xs));

    xs[0] = {{({0} + "")[{0}[0]]}[0]}
    println(string_of_2array(xs));

    xs = xs + xs + xs;
    println(string_of_2array(xs));
}
