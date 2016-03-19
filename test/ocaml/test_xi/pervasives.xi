use io;

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

string_of_bool(x: bool) : int[] {
    if (x) {
        return "true";
    } else {
        return "false";
    }
}

string_of_1array(x: int[]) : int[] {
    s: int[] = "{";
    i: int = 0;
    while (i < length(x)) {
        s += unparseInt(x[i]) + ",";
    }
    s = s + "}"
    return s;
}

string_of_2array(x: int[][]) : int[] {
    s: int[] = "{";
    i: int = 0;
    while (i < length(x)) {
        s += string_of_1array(x[i]) + ",";
    }
    s = s + "}"
    return s;
}

string_of_3array(x: int[][][]) : int[] {
    s: int[] = "{";
    i: int = 0;
    while (i < length(x)) {
        s += string_of_2array(x[i]) + ",";
    }
    s = s + "}"
    return s;
}

string_of_4array(x: int[][][][]) : int[] {
    s: int[] = "{";
    i: int = 0;
    while (i < length(x)) {
        s += string_of_3array(x[i]) + ",";
    }
    s = s + "}"
    return s;
}

string_of_5array(x: int[][][][][]) : int[] {
    s: int[] = "{";
    i: int = 0;
    while (i < length(x)) {
        s += string_of_4array(x[i]) + ",";
    }
    s = s + "}"
    return s;
}

int_of_string(s: int[]) : int {
    return parseInt(s);
}

string_of_int(x: int) : int[] {
    return unparseInt(x);
}
