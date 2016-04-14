from itertools import product

################################################################################
# constants
################################################################################
ARITHMETICS = ["-","*","*>>","/","%","+"]
COMPARISONS = ["<","<=",">=",">","==","!="]
LOGICS = ["==","!=","&","|"]
BOOLS = ["true", "false"]
# NUMS = [-9223372036854775808, 9223372036854775807] + range(-5, 5)
NUMS = [-2, -1, 0, 1, 2, 3, 4, 5, 8, 9]
VARS = ["x", "y", "z"]

################################################################################
# helpers
################################################################################
def chunks(l, n):
    for i in range(0, len(l), n):
        yield l[i:i+n]

def flatten(ls):
    return [x for l in ls for x in l]

################################################################################
# pretty printing
################################################################################
def println(x):
    return "println({})".format(x)

def string_of_bool(x):
    return "string_of_bool({})".format(x)

def string_of_int(x):
    return "string_of_int({})".format(x)

def mainify(ls):
    return ("main(_: int[][]) {\n" +
            "\n".join(["{}:int = 0".format(v) for v in VARS]) +
            "\n".join(flatten(ls)) +
            "\n}")

def exprs():
    for (x, o, y) in product(NUMS, ARITHMETICS, NUMS):
        if (o == "%" or o == "/") and y == 0:
            continue
        yield [println('"{} {} {}"'.format(x, o, y)),
               println(string_of_int("{} {} {}".format(x, o, y)))]

    for (v, x, o, y) in product([VARS[0]], NUMS, ARITHMETICS, NUMS):
        if (o == "%" or o == "/") and y == 0:
            continue
        yield [println('"{} = {}"'.format(v, x)),
               "{} = {}".format(v, x),
               println('"{} {} {}"'.format(v, o, y)),
               println(string_of_int("{} {} {}".format(v, o, y))),
               println('"{} {} {}"'.format(y, o, v)),
               println(string_of_int("{} {} {}".format(y, o, v)))]

def main():
    chunk_size = 50
    filename = "xisrc/ours/difftests/zinger"
    for (i, chunk) in enumerate(chunks(list(exprs()), chunk_size)):
        with open("{}{}.xi".format(filename, i), "w") as f:
            f.write(mainify(chunk))

if __name__ == "__main__":
    main()
