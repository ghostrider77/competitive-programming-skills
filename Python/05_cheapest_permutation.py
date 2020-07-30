import sys
import itertools as it


def convert_to_intlist(line):
    return [int(elem) for elem in line.split()]


def read_matrix(reader, n):
    matrix = []
    for _ in range(n):
        line = next(reader)
        row = convert_to_intlist(line)
        matrix.append(row)
    return matrix


def find_cheapest_permutation(matrix, n):
    def calc_cost(perm):
        cost = 0
        for i, j in zip(perm, perm[1:]):
            cost += matrix[i][j]
        return cost

    min_perm = list(range(n))
    min_cost = calc_cost(min_perm)
    perms = it.permutations(min_perm)
    for p in perms:
        cost = calc_cost(p)
        if cost < min_cost:
            min_cost = cost
            min_perm = p
    return min_perm


def main():
    reader = sys.stdin
    n = int(next(reader))
    matrix = read_matrix(reader, n)
    result = find_cheapest_permutation(matrix, n)
    print(' '.join([str(elem + 1) for elem in result]))


if __name__ == '__main__':
    main()
