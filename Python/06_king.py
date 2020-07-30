import math
import sys


def convert_to_intlist(line):
    return [int(elem) for elem in line.split()]


def maximum_number_of_kings(r, c):
    empty_cells = math.ceil(r / 3) * math.ceil(c / 3)
    return r*c - empty_cells


def main():
    reader = sys.stdin
    r, c = convert_to_intlist(next(reader))
    result = maximum_number_of_kings(r, c)
    print(result)


if __name__ == '__main__':
    main()
