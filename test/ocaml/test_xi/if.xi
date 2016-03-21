main(_: int[][]) {
	// print 2
	{
		x: int = 3;
		if (x == 3) {
			x = 2;
		} else {
			x = 4;
		}
		print(string_of_int(x));
	}
	// print 3
	{
		x:int = 3;
		if (x != 3) {
			x = 4;
		}
		print(string_of_int(x));
	}
	// print 2
	// print 9
	{
		x: bool[] = {true, false, false, false, true, true};
		y: int = 3;
		v: int = 0;
		w: int = 0;
		if (x[0]) {
			if (x[3]) {
				w = 3;
				v = 6;
			} else {
				w = 2;
				v = 9;
			}
		} else {
			if (x[4]) {
				w = -1;
				v = 1;
			} else {
				w = 11;
				v = 4;
			}
		}
		print(string_of_int(w));
		print(string_of_int(v));
	}
}
