main(_: int[][]) {
    one:  int[]
    two:  int[][]
    three:int[][][]
    four: int[][][][]
    five: int[][][][][]

    one   = {1} + {1};
    two   = {one} + {one};
    three = {two} + {two} + {two};
    four  = "" + "" + {three} + {three} + {three} + {three};
    five  = "" + {four};

    println(string_of_5array(five));
}
