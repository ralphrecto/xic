from itertools import product

################################################################################
# constants
################################################################################
ARITHMETICS = ["-","*","*>>","/","%","+"]
COMPARISONS = ["<","<=",">=",">","==","!="]
LOGICS = ["==","!=","&","|"]
BOOLS = ["true", "false"]
NUMS = [-9223372036854775808, 9223372036854775807]
NUMS = NUMS + [-2, -1, 0, 1, 2, 3]
OFFS = [1,4]
VARS = ["x", "y"]

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
    # k op k
    for (x, o, y) in product(NUMS, ARITHMETICS, NUMS):
        if (o == "%" or o == "/") and y == 0:
            continue
        yield [println('"{} {} {}"'.format(x, o, y)),
               println(string_of_int("{} {} {}".format(x, o, y)))]

    for (x, o, y) in product(NUMS, COMPARISONS, NUMS):
        yield [println('"{} {} {}"'.format(x, o, y)),
               println(string_of_bool("{} {} {}".format(x, o, y)))]

    for (x, o, y) in product(BOOLS, LOGICS, BOOLS):
        yield [println('"{} {} {}"'.format(x, o, y)),
               println(string_of_bool("{} {} {}".format(x, o, y)))]

    # x op k, k op x
    for (v, x, o, y) in product([VARS[0]], NUMS, ARITHMETICS, NUMS):
        asms = []
        asms.append(println('"{} = {}"'.format(v, x)))
        asms.append("{} = {}".format(v, x))
        if not ((o == "%" or o == "/") and y == 0):
            asms.append(println('"{} {} {}"'.format(v, o, y)))
            asms.append(println(string_of_int("{} {} {}".format(v, o, y))))
        if not ((o == "%" or o == "/") and x == 0):
            asms.append(println('"{} {} {}"'.format(y, o, v)))
            asms.append(println(string_of_int("{} {} {}".format(y, o, v))))
        yield asms

    for (v, x, o, y) in product([VARS[0]], NUMS, COMPARISONS, NUMS):
        asms = []
        asms.append(println('"{} = {}"'.format(v, x)))
        asms.append("{} = {}".format(v, x))
        asms.append(println('"{} {} {}"'.format(v, o, y)))
        asms.append(println(string_of_bool("{} {} {}".format(v, o, y))))
        asms.append(println('"{} {} {}"'.format(y, o, v)))
        asms.append(println(string_of_bool("{} {} {}".format(y, o, v))))
        yield asms

    for (v, x, o, y) in product([VARS[0]], BOOLS, LOGICS, BOOLS):
        asms = []
        asms.append(println('"{} = {}"'.format(v, x)))
        asms.append("{} = {}".format(v, x))
        asms.append(println('"{} {} {}"'.format(v, o, y)))
        asms.append(println(string_of_bool("{} {} {}".format(v, o, y))))
        asms.append(println('"{} {} {}"'.format(y, o, v)))
        asms.append(println(string_of_bool("{} {} {}".format(y, o, v))))
        yield asms

    # leaq-case 1
    for (reg1, x, reg2, y, off, k) in product(VARS, NUMS, VARS, NUMS, OFFS, NUMS):
        asms = []
        asms.append(println('"{} = {}"'.format(reg1, x)))
        asms.append("{} = {}".format(reg1, x))
        asms.append(println('"{} = {}"'.format(reg2, y)))
        asms.append("{} = {}".format(reg2, y))

        asms.append(println('"{} * {} + {} + {}"'.format(reg1, off, reg2, k)))
        asms.append(println(string_of_int("{} * {} + {} + {}".format(reg1, off, reg2, k))))
        asms.append(println('"{} * {} + {} - {}"'.format(reg1, off, reg2, k)))
        asms.append(println(string_of_int("{} * {} + {} - {}".format(reg1, off, reg2, k))))
        yield asms


def main():
    chunk_size = 10
    filename = "xisrc/ours/difftests/zinger"
    for (i, chunk) in enumerate(chunks(list(exprs()), chunk_size)):
        with open("{}{}.xi".format(filename, i), "w") as f:
            f.write(mainify(chunk))

if __name__ == "__main__":
    main()
