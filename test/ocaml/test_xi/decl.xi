main(args: int[][]) {
	//should return 2
	{
		x: int[] = {1,2,3};
		print(string_of_int(x[1]));
	}
	// should return 0
	{
		x: int[][];
		print(string_of_int(length(x)));
	}
	{
		x: int[][] = {{1,2},{3}};
		print(string_of_1array(x[1]));
	}

	{
		x: int[3];
		print(string_of_int(length(x)));
	}

	{
		x: int[3][4];
		print(string_of_int(length(x[0])));
	}
	{
		x:int = 3;
		print(string_of_int(x));
	}
	{
		x:bool = true;
		print(string_of_bool(true));
	}
	{
		x:int, y:int;
		x = 3;
		y = 3;
		print(string_of_int(x));
		print(string_of_int(y));
	}
	{
		x:int, y:bool;
		x = 3;
		y = false;
		print(string_of_int(x));
		print(string_of_bool(y));
	}
	{
		x:int, y:int, z:bool;
		x = 3;
		y = 3;
		z = false;
		print(string_of_int(x));
		print(string_of_int(y));
		print(string_of_bool(z));
	}
	{
		x: int;
		x = 3;
		print(string_of_int(x));
	}
	{
		x:int, y:int[];
		x = 3;
		y = {1,3};
		print(string_of_int(x));
		print(string_of_1array(y));
	}
	{
		x:int[], y:int[][];
		x = {1,2};
		y = {{1,3}, {2,3}};
		print(string_of_1array(x));
		print(string_of_2array(y));
	}
	// should throw error
	{
		x: int[];
		print(string_of_int(x[0]));
	}
	// what does this return? 0?
	{
		x: int;
		print(string_of_int(x));
	}
	// similar case as to stmt3.xi
	{
		x: bool;
		print(string_of_bool(x));
	}
	// making sure arrays are filled in properly
	{
		x: int[];
		x = {1, 2, 3};
		print(string_of_int(x[1]));
	}
	//array declared with sizes
	{
		x: int[3];
		print(string_of_int(x[0]));
	}
	//array declared with sizes
	//should say invalid memory
	{
		x: int[3];
		print(string_of_int(x[3]));
	}
	//should be invalid
	{
		x: int[3];
		x[3] = 3;	
	}
	//should return 0
	{
		x : int[];
		print(string_of_int(length(x)));
	}
}
