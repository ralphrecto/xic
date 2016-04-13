test1() {
    x:int = 3;
    y:int = 4;
    println(unparseInt(x+y));
}

test2() {
  x:int = 3;
  y:int = 4;
  println(unparseInt(x-y));

}

test3() {
  x:bool = true;
  y:bool = false;
  println(string_of_bool(x&y));
}

test4() {
  x:bool = true;
  y:bool = false;
  println(string_of_bool(x|y));
}

test5() {
  x:int = 3;
  y:int = 4;
  println(string_of_bool(x==y));
}

test6() {
  x:int = 3;
  y:int = 4;
  println(string_of_bool(x!=y));
}

test7() {
  x:int = 3;
  y:int = 4;
  println(string_of_bool(x==y));
}

test8() {
  x:int = 3;
  y:int = 4;
  println(string_of_bool(x<y));
}

test9() {
  x:int = 3;
  y:int = 4;
  println(string_of_bool(x>y));
}

test10() {
  x:int = 3;
  y:int = 4;
  println(string_of_bool(x<=y));
}

test11() {
  x:int = 3;
  y:int = 4;
  println(string_of_bool(x>=y));
}

test12() {
  println(unparseInt(3));
}

test13() {
  println("3");
}

test14() {
  x:int[] = {1,2,3};
  println(string_of_1array(x));
}

word(x:int[]){
    z:int = 0;
    w:int[] = "{";
    while (z < length(x)) {
        t: int[] = unparseInt(x[z]);
        w = w + t + ",";
        z = z+1;
    }
    w = w + "}";
    println(w);
}

what(x:int[][]){
  y:int = 0;
  println ("{");
  while (y < length(x)) {
    word(x[y]);
    y = y+1;
  }
  println("}");
}

test15() {
  x:int[][] = {{1},{1},{1}};
  what(x);
}

test16() {
  x:int[][][] = {
      {{1,2,3},{1,2,3},{1,2,3}},
      {{1,2,3},{1,2,3},{1,2,3}},
      {{1,2,3},{1,2,3},{1,2,3}}
  }
  println(string_of_3array(x));
}

test17() {
  x:int[][][][] = {
      {{{1,2,3},{1,2,3},{1,2,3}},{{1,2,3},{1,2,3},{1,2,3}},{{1,2,3},{1,2,3},{1,2,3}}},
      {{{1,2,3},{1,2,3},{1,2,3}},{{1,2,3},{1,2,3},{1,2,3}},{{1,2,3},{1,2,3},{1,2,3}}},
      {{{1,2,3},{1,2,3},{1,2,3}},{{1,2,3},{1,2,3},{1,2,3}},{{1,2,3},{1,2,3},{1,2,3}}}
  }
  println(string_of_4array(x));
}

test18() {
  x:int[][][] = {
      {{1,2,3},{1,2,3},{1,2,3}},
      {{1,2,3},{1,2,3},{1,2,3}},
      {{1,2,3},{1,2,3},{1,2,3}}
  }
  println(string_of_3array(x));
}


main(_: int[][]) {
    test1()
    test2()
    test3()
    test4()
    test5()
    test6()
    test7()
    test8()
    test9()
    test10()
    test11()
    test12()
    test13()
    test14()
    test15()
    test16()
    test17()
    test18()
}
