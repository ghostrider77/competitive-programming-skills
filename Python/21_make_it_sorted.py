import sys

LIMIT = 5 # 1000


def convert_to_intlist(line):
    return [int(elem) for elem in line.split()]


def minimum_steps_to_make_array_sorted(array, n):
    table = [[0] * LIMIT for _ in range(n)]
    elem = array[0]
    for x in range(1, LIMIT+1):
        table[0][x-1] = abs(x - elem)

    for ix in range(1, n):
        elem = array[ix]
        m = table[ix-1][0]
        for y in range(1, LIMIT+1):
            m = min(m, table[ix-1][y-1])
            table[ix][y-1] = m + abs(y - elem)
    return min(table[n-1][x-1] for x in range(1, LIMIT+1))


def main():
    reader = sys.stdin
    n = int(next(reader))
    array = convert_to_intlist(next(reader))
    result = minimum_steps_to_make_array_sorted(array, n)
    print(result)


if __name__ == '__main__':
    main()
