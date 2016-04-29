string_of_1barray(x: bool[]) : int[] {
    s: int[] = "{";
    i: int = 0;
    while (i < length(x)) {
        s = s + string_of_bool(x[i]) + ",";
        i = i + 1;
    }
    s = s + "}"
    return s;
}

is_prime(x: int) : bool {
    if (x <= 1) {
        return false;
    }
    i: int = 2;
    while (i < x) {
        if (x % i == 0) {
            return false;
        }
        i = i + 1
    }
    return true;
}

main(_: int[][]) {
    xs: int[] = range(0, 10);
    bs: bool[length(xs)];
    i: int = 0;
    while (i < length(xs)) {
        bs[i] = is_prime(xs[i]);
        i = i + 1;
    }
    println(string_of_1barray(bs));
}
