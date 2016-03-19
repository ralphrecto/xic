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

main() {
    xs: int[] = range(0, 50);
    bs: bool[length(xs)];
    i: int = 0;
    while (i < length(xs)) {
        bs[i] = is_prime(xs[i]);
    }
    print
}
