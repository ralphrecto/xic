use io
use conv

usage() {
    println("Please specify the input size")
}

main(args:int[][]) {
    print("input m: ")
    sm: int[] = readln()
    m: int, _ = parseInt(sm)
    print("input n: ")
    sn: int[] = readln()
    n: int, _ = parseInt(sn)

    r: int = Ack(m, n)
    
    print("Ack(")
    print(unparseInt(m))
    print(",")
    print(unparseInt(n))
    print("): ")
    print(unparseInt(r))
    println("")
}

Ack(m:int, n:int):int {
    if (m == 0) { return n+1 }
    else if (n == 0) { return Ack(m-1, 1) }
    else { return Ack(m-1, Ack(m, n-1)) }
}
