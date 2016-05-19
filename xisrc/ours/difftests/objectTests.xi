class Point {
  x,y:int

  move(dx:int, dy:int) {
    x = x + dx
    y = y + dy
  }
  coords():int,int{
    return x, y
  }
  add(p:Point) : Point {
    return createPoint(x + p.x, y + p.y)
  }
  initPoint(x0:int, y0:int) : Point {
    x = x0
    y = y0
    return this
  }
  clone():Point { return createPoint(x, y) }
  equals(p:Point):bool { return this == p }

  toString():int[] {
    return "Point (" + (string_of_int (this.x)) +
      ", " + (string_of_int (this.y)) + ")";
  }
}

createPoint(x:int, y:int) : Point {
  return new Point.initPoint(x, y)
}

main(_:int[][]) {
  x:Point = createPoint(0, 0);
  y:Point = createPoint(1, 1);
  x2:Point = x.clone();
  println(x.toString());
  println(string_of_bool(x.equals(x)));
  println(string_of_bool(x.equals(y)));
  println(string_of_bool(y.equals(x)));
  println(string_of_bool(x2.equals(x)));
  println(string_of_bool(x.equals(x2)));

  x2.x = y.x
  y.x = x.x
  x.x = x2.x
  println(string_of_bool(x.x == y.y));
  println(string_of_bool(x.y == y.x));
}
