import sys
import math


def convert_to_intlist(line):
    return [int(elem) for elem in line.split()]


def main():
    reader = sys.stdin
    x, y = convert_to_intlist(next(reader))
    result = x // y if x % y == 0 else math.ceil(x / y)
    print(result)


if __name__ == '__main__':
    main()
