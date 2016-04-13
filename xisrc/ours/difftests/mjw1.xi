empty_first(xs: int[][]) {
    xs[0] = {};
}

increment(xs: int[]) : int[] {
    i: int = 0;
    while (i < length(xs)) {
       xs[i] = xs[i] + 1;
       i = i + 1;
    }
    return xs;
}

main(args:int[][]) {
    {
        xs: int[][] = {{}, {1}, {1,2}};
        println(string_of_2array(xs));

        xs[2] = xs[0];
        println(string_of_2array(xs));

        xs[1] = xs[1] + {xs[1][0]};
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

        one = "" + "" + {1} + "";
        two = {} + {one} + {one, one} + {one} + {};
        three = {{one}} + {} + {two} + {two, {one}} + {two} + {} + {{two[0]}};
        four = {three} + {{two}} + {{{one}}};
        five = {four} + "" + "" + {four};

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

        // println(string_of_1array(one));
        // println(string_of_2array(two));
        // println(string_of_3array(three));
        // println(string_of_4array(four));
        // println(string_of_5array(five));
    }

    {
        xs: int[] = {1, 2};
        ys: int[] = {2, 1, 3, 1};
        println(string_of_1array(xs));
        println(string_of_1array(ys));

        temp: int[]
        temp = xs
        xs = ys
        ys = temp
        println(string_of_1array(xs));
        println(string_of_1array(ys));

        xs = ys;
        println(string_of_1array(xs));
        println(string_of_1array(ys));

        ys[0] = 42;
        println(string_of_1array(xs));
        println(string_of_1array(ys));

        xs[1] = 43;
        println(string_of_1array(xs));
        println(string_of_1array(ys));
    }

    {
        xs:int[] = range(0, 10);
        println(string_of_1array(xs))
        _ = increment(xs);
        println(string_of_1array(xs))
        println(string_of_1array(increment(xs)))
        println(string_of_1array(increment({1, 2, 3, 4})))
    }
}
