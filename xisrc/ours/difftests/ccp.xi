// TO REALLY TEST CCP LOOK AT THE CFG THAT'S BEING CREATED
// OR THE IR

test1() {
  x:int = 1
  y:int = 2
  if (x < y) {
    println("hello")
  } else {
    println("world")
  }
}

foo(_:int):int {
  println("what")
  return 4;
}

test2() {
  x:int = 1
  y:int = 2
  _:int = foo(x+y)
}

test3() {
  x:int = 1
  y:int = 2
  z:int[] = {1,2}
  a:int = z[x+y]
}

test4() {
  x:int = 1
  y:int = 2
  z:int = 3
  a:int = 4
  b:int = 5
  c:int = 6
  d:int = 7
  if (x < d) {
    if (y < d) {
      if (d < c + x) {
      } else {
        if (b < z) {
          println ("what")
        } else {
          if (z+b+d < a+x+y){
            println ("lol")
          } else {
            println ("coffee")
          }
        }
      }
    } else {
      if (a+y+d < a+a+a+a+a) {
        println(unparseInt(a))
      } else {
        println(unparseInt(a+b));
      }
    }
  } else {
    if (b+d+y < 20) {
      if (15 <= d+b+a) {
        println("i have no idea what this is anymore")
      } else {
        if (x+z+d < y+a+b+c) {
          println("hola")
        } else {
          println("lincoln")
        }
      }
    } else {
      if (b+b+b < y+y+y) {
        println(unparseInt(b));
        println(unparseInt(y));
      } else {
        if (c-c*c/c < y *>> y +y % y) {
          println ("what am i doing")
        } else {
          println ("orange")
        }
      }
    }
  }
}

test5() {
  x:int = 1
  y:int = 2
  d:int;
  if (x < y) {
    d = 3;
  } else {
    d = foo(x);
  }

  if (d < 10) {
    println("hello")
  } else {
    println("what")
  }
}

test6() {
  x:int = 1
  y:int = 2
  d:int;
  z:int[] = {1,2}
  if (x < z[x]) {
    d = 3;
  } else {
    d = foo(x);
  }

  if (d < 10) {
    println("hello")
  } else {
    println("what")
  }
}

main(_: int[][]) {
    test1();
    test2();
    test3();
    test4();
    test5();
    test6();
}
