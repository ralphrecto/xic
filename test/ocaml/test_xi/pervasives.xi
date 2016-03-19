use io;

// parsing and pretty printing
string_of_bool(x: bool) : int[] {
    if (x) {
        return "true";
    } else {
        return "false";
    }
}

int_of_string(s: int[]) : int {
    return parseInt(s);
}

string_of_int(x: int) : int[] {
    return unparseInt(x);
}

string_of_1array(x: int[]) : int[] {
    s: int[] = "{";
    i: int = 0;
    while (i < length(x)) {
        s = s + unparseInt(x[i]) + ",";
    }
    s = s + "}"
    return s;
}

string_of_2array(x: int[][]) : int[] {
    s: int[] = "{";
    i: int = 0;
    while (i < length(x)) {
        s = s + string_of_1array(x[i]) + ",";
    }
    s = s + "}"
    return s;
}

string_of_3array(x: int[][][]) : int[] {
    s: int[] = "{";
    i: int = 0;
    while (i < length(x)) {
        s = s + string_of_2array(x[i]) + ",";
    }
    s = s + "}"
    return s;
}

string_of_4array(x: int[][][][]) : int[] {
    s: int[] = "{";
    i: int = 0;
    while (i < length(x)) {
        s = s + string_of_3array(x[i]) + ",";
    }
    s = s + "}"
    return s;
}

string_of_5array(x: int[][][][][]) : int[] {
    s: int[] = "{";
    i: int = 0;
    while (i < length(x)) {
        s = s + string_of_4array(x[i]) + ",";
    }
    s = s + "}"
    return s;
}

string_of_1barray(x: int[]) : int[] {
    s: int[] = "{";
    i: int = 0;
    while (i < length(x)) {
        s = s + string_of_bool(x[i]) + ",";
    }
    s = s + "}"
    return s;
}

string_of_2barray(x: int[][]) : int[] {
    s: int[] = "{";
    i: int = 0;
    while (i < length(x)) {
        s = s + string_of_1barray(x[i]) + ",";
    }
    s = s + "}"
    return s;
}

string_of_3barray(x: int[][][]) : int[] {
    s: int[] = "{";
    i: int = 0;
    while (i < length(x)) {
        s = s + string_of_2barray(x[i]) + ",";
    }
    s = s + "}"
    return s;
}

string_of_4barray(x: int[][][][]) : int[] {
    s: int[] = "{";
    i: int = 0;
    while (i < length(x)) {
        s = s + string_of_3barray(x[i]) + ",";
    }
    s = s + "}"
    return s;
}

string_of_5barray(x: int[][][][][]) : int[] {
    s: int[] = "{";
    i: int = 0;
    while (i < length(x)) {
        s = s + string_of_4barray(x[i]) + ",";
    }
    s = s + "}"
    return s;
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

