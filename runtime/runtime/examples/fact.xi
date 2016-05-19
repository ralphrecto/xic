use conv
use io

factorial(i: int): int {
    if (i <= 1) {
        return 1
    } else {
        return i * factorial(i - 1)
    }
}

main(arg: int[][]) {

    while (!eof()) {
        print("Number?")
        input: int[] = readln()
        num: int, ok: bool = parseInt(input)

        if (ok) {
            val: int = factorial(num)
            print(unparseInt(num))
            print("! is ")
            println(unparseInt(val))
        }
    }
}
