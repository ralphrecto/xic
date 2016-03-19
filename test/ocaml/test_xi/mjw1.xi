empty_first(xs: int[][]) {
    xs[0] = {};
}

main() {
    {
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

    {
        xs: int[2][3];
        xs[0][0] = 0;
        xs[0][1] = 1;
        xs[0][2] = 2;
        xs[1][0] = 3;
        xs[1][1] = 4;
        xs[1][2] = 5;
        println(string_of_2array(xs));
    }

    {
        xs: int[2][3];
        xs[0] = {};
        xs[1] = {1, 2};
        println(string_of_2array(xs));
    }

    {
        one: int[];
        two: int[][];
        three: int[][][];
        four: int[][][][];
        five: int[][][][][];

        one = "" + {1} + "";
        two = {} + {one} + {one, one} + {one} + {};
        three = {{one}} + {} + {two} + {two, {one}} + {two} + {} + {two[0]};
        four = {three} + {{two}} + {{{one}}};
        five = "" + "" + {four};

        println(string_of_1array(one));
        println(string_of_2array(two));
        println(string_of_3array(three));
        println(string_of_4array(four));
        println(string_of_5array(five));

        one = one + one;
        two = two + two + two;
        three = three;
        four = four + four + four;
        five = five;

        println(string_of_1array(one));
        println(string_of_2array(two));
        println(string_of_3array(three));
        println(string_of_4array(four));
        println(string_of_5array(five));
    }
}
