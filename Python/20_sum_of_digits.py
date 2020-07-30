import sys


def convert_to_intlist(line):
    return [int(elem) for elem in line.split()]


def solve_sum_of_digits_problem(n, length):
    table = [[0] * length for _ in range(n+1)]

    if length == 1:
        return 1 if 0 <= n <= 9 else 0


    for ix in range(min(n, 9)+1):
            table[ix][1] = 1

    for jy in range(1, length):
        table[0][jy] = 1

    for jy in range(2, length):
        for ix in range(1, n):
            table[ix][jy] = sum(table[ix-k][jy-1] for k in range(10) if ix >= k)

    return sum(table[n-k][length-1] for k in range(1, 10) if n >= k)


def main():
    reader = sys.stdin
    n, length = convert_to_intlist(next(reader))
    result = solve_sum_of_digits_problem(n, length)
    print(result)


if __name__ == '__main__':
    main()
