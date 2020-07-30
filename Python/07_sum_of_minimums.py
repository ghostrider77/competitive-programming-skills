import sys


INFINITY = 1000000


def convert_to_intlist(line):
    return [int(elem) for elem in line.split()]


def sum_of_minimums(array, n):
    s = 0
    for left in range(n):
        m = INFINITY
        for elem in array[left:]:
            if elem < m:
                m = elem
            s += m
    return s


def main():
    reader = sys.stdin
    n = int(next(reader))
    array = convert_to_intlist(next(reader))
    result = sum_of_minimums(array, n)
    print(result)


if __name__ == '__main__':
    main()
