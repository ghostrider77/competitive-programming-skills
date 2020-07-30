import sys


def convert_to_intlist(line):
    return [int(elem) for elem in line.split()]


def find_first_occurrence(x, y, z):
    if z == 0:
        return 0
    if z == x:
        return 1
    difference = x - y
    if difference == 0 and z != x:
        return -1
    return handle_nontrivial_cases(y, difference, z)


def handle_nontrivial_cases(y, difference, z):
    k, r = divmod(z - y, difference)
    if r == 0 and k > 0:
        return 2*k - 1
    k, r = divmod(z, difference)
    if r == 0 and k > 0:
        return 2*k
    return -1


def main():
    reader = sys.stdin
    elems = convert_to_intlist(next(reader))
    result = find_first_occurrence(*elems)
    print(result)


if __name__ == '__main__':
    main()
