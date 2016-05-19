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

class Color {
  r, g, b: int

  toString():int[] {
    return "Color (" + (string_of_int(r)) + ", " +
      (string_of_int(g)) + ", " +
      (string_of_int(b)) + ")";
  }
}
 
class ColoredPoint extends Point {
  col:Color
  color(): Color { return col }
  x, y:int

  initColoredPoint(x0:int, y0:int, c:Color):ColoredPoint {
    col = c
    _ = initPoint(x0, y0)
    x = x0
    y = y0
    return this
  }


  toString():int[] {
    return "ColoredPoint(" + (string_of_int (this.x)) + ", " +
      (string_of_int (this.y)) + ", " + this.col.toString() + ")";
  }
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

  color1:Color;
  color1.r = 255
  color1.g = 255
  color1.b = 0
  cp:ColoredPoint = new ColoredPoint.initColoredPoint(5, 5, color1);
  println(cp.toString());
}
