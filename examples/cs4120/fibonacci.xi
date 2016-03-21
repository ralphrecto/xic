use io
use conv

fib(i:int) : int {
	if (i < 2) {
		return i
	} else {
		return fib(i-1) + fib(i-2)
	}
}

main(args:int[][]) {
	print("Please enter a positive number : ")
	input: int[] = readln()

	value:int, valid:bool = parseInt(input)
	if (!valid) {
		println("Invalid input!")
		return
	}

	println(unparseInt(fib(value)))
}
