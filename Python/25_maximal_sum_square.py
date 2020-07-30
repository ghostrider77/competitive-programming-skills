import sys


def convert_to_intlist(line):
    return [int(item) for item in line.split()]


def read_matrix(reader, n):
    matrix = []
    for _ in range(n):
        row = convert_to_intlist(next(reader))
        matrix.append(row)
    return matrix


def maximal_sum_square_submatrix(matrix, n, k):
    prefix_sum = [[0] * (n+1) for _ in range(n+1)]
    for ix in range(1, n+1):
        for jy in range(1, n+1):
            ps = (prefix_sum[ix-1][jy] + prefix_sum[ix][jy-1] - prefix_sum[ix-1][jy-1] + matrix[ix-1][jy-1])
            prefix_sum[ix][jy] = ps

    result = 0
    for ix in range(n-k+1):
        for jy in range(n-k+1):
            s = prefix_sum[ix+k][jy+k] - prefix_sum[ix+k][jy] - prefix_sum[ix][jy+k] + prefix_sum[ix][jy]
            if s > result:
                result = s
    return result


def main():
    reader = sys.stdin
    n, k = convert_to_intlist(next(reader))
    matrix = read_matrix(reader, n)
    result = maximal_sum_square_submatrix(matrix, n, k)
    print(result)


if __name__ == "__main__":
    main()
