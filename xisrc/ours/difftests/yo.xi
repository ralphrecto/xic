class Add {
  call() : int {
    return 1;
  }
}

sum(foldf:Add) : int {
    m:int[10]
    acc: int = 0
    i: int = 0
    len: int = 10
    while (i < len) {
        x:int = m[0]
        acc = x + foldf.call();
        i = i + 1
    }
    return acc;
}

main(_:int[][]) {
    println(unparseInt(sum(new Add)));
}
