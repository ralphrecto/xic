from itertools import product

ARITHMETICS = ["-","*","*>>","/","%","+"]
COMPARISONS = ["<","<=",">=",">","==","!="]
LOGICS = ["==","!=","&","|"]
BOOLS = ["true", "false"]
NUMS = [-9223372036854775808, 9223372036854775807] + range(-5, 5)
VARS = ["x", "y", "z"]

def println(x):
    return "println({})".format(x)

def string_of_bool(x):
    return "string_of_bool({})".format(x)

def string_of_int(x):
    return "string_of_int({})".format(x)

def main():
    print "main(_:int[][]) {"

    for (x, o, y) in product(NUMS, ARITHMETICS, NUMS):
        if (o == "%" or o == "/") and y == 0:
            continue
        print println('"{} {} {}"'.format(x, o, y))
        print println(string_of_int("{} {} {}".format(x, o, y)))

    for (v, x, o, y) in product(VARS, NUMS, ARITHMETICS, NUMS):
        if (o == "%" or o == "/") and y == 0:
            continue
        print println('"{} {} {}"'.format(x, o, y))
        print "{} = {}".format(v, x)
        print println(string_of_int("{} {} {}".format(v, o, y)))
        print println(string_of_int("{} {} {}".format(y, o, v)))

    print "}"

if __name__ == "__main__":
    main()
