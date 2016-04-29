use io;
use conv;

// parsing and pretty printing
string_of_bool(x: bool) : int[] {
    if (x) {
        return "true";
    } else {
        return "false";
    }
}

int_of_string(s: int[]) : int {
    x:int, b:bool = parseInt(s);
    return x;
}

string_of_int(x: int) : int[] {
    return unparseInt(x);
}

string_of_1array(x: int[]) : int[] {
    s: int[] = "{";
    i: int = 0;
    while (i < length(x)) {
        s = s + unparseInt(x[i]) + ",";
        i = i + 1;
    }
    s = s + "}"
    return s;
}

string_of_2array(x: int[][]) : int[] {
    s: int[] = "{";
    i: int = 0;
    while (i < length(x)) {
        s = s + string_of_1array(x[i]) + ",";
        i = i + 1;
    }
    s = s + "}"
    return s;
}

string_of_3array(x: int[][][]) : int[] {
    s: int[] = "{";
    i: int = 0;
    while (i < length(x)) {
        s = s + string_of_2array(x[i]) + ",";
        i = i + 1;
    }
    s = s + "}"
    return s;
}

string_of_4array(x: int[][][][]) : int[] {
    s: int[] = "{";
    i: int = 0;
    while (i < length(x)) {
        s = s + string_of_3array(x[i]) + ",";
        i = i + 1;
    }
    s = s + "}"
    return s;
}

string_of_5array(x: int[][][][][]) : int[] {
    s: int[] = "{";
    i: int = 0;
    while (i < length(x)) {
        s = s + string_of_4array(x[i]) + ",";
        i = i + 1;
    }
    s = s + "}"
    return s;
}

print_1array(x: int[]) {
    i: int = 0
    println("{")
    while (i < length(x)) {
        println(unparseInt(x[i]))
        i = i + 1
    }
    println("}")
}

print_2array(x: int[][]) {
    i: int = 0
    println("{")
    while (i < length(x)) {
        print_1array(x[i])
        i = i + 1
    }
    println("}")
}

print_3array(x: int[][][]) {
    i: int = 0
    println("{")
    while (i < length(x)) {
        print_2array(x[i])
        i = i + 1
    }
    println("}")
}

print_4array(x: int[][][][]) {
    i: int = 0
    println("{")
    while (i < length(x)) {
        print_3array(x[i])
        i = i + 1
    }
    println("}")
}

print_5array(x: int[][][][][]) {
    i: int = 0
    println("{")
    while (i < length(x)) {
        print_4array(x[i])
        i = i + 1
    }
    println("}")
}

// Misc
range(low: int, high: int) : int[] {
    xs: int[] = {};
    while (low < high) {
        xs = xs + {low};
        low = low + 1;
    }
    return xs;
}

// Checks for structural equality of two arrays, not physical equality
arr_eq(x: int[], y: int[]) : bool {
    if (length(x) != length(y)) {
        return false;
    }
    i: int = 0;
    while (i < length(x)) {
        if (x[i] != y[i]) {
            return false;
        }
        i = i + 1;
    }
    return true;
}

