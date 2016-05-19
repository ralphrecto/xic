class Pure {
  bar ():int,int{
    return 1,2
  }
}

class Leaf extends Pure {
}


main (_:int[][]){
  x : Leaf = new Leaf;
  a:int, b:int = x.bar();
  println(unparseInt(a));
  println(unparseInt(b));
}
