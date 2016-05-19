class Hello {
  foo(x1:int, x2:int):int{
    return x1+x2
  }
}

class World extends Hello {
  foo(x1:int, x2:int):int{
    return 3
  }
  bar (x1:int, x2:int, x3:int):int,int{
    return x1, x2+x3
  }
}

class Pure extends World {
  bar (x1:int, x2:int, x3:int):int,int{
    return x1+x2, x3
  }
  lol (x1:int, x2:bool):int{
    if (x2) {
      return 3;
    } else {
      return x1
    }
  }
}

class Leaf extends Pure {
  foo(x1:int, x2:int):int{
    return 54
  }
}


main (_:int[][]){
  x : Leaf = new Leaf;
  println(unparseInt(x.foo(1,2)));
  println(unparseInt(x.lol(3,true)));
  a:int, b:int = x.bar(4,5,6);
  println(unparseInt(a));
  println(unparseInt(b));
}
