import sys


def convert_to_intlist(line):
    return [int(item) for item in line.split()]


def calc_cheapest_matrix_multiplications(sizes, nr_matrices):
    d = {}
    def solve(i, j):
        if j == i + 1:
            return 0
        if (i, j) in d:
            return d[(i, j)]

        min_cost = float('inf')
        for k in range(i+1, j):
            min_cost = min(min_cost, solve(i, k) + solve(k, j) + sizes[i]*sizes[j]*sizes[k])
        d[(i, j)] = min_cost
        return min_cost

    return solve(0, nr_matrices)


def main():
    reader = sys.stdin
    nr_matrices = int(next(reader))
    sizes = tuple(convert_to_intlist(next(reader)))
    result = calc_cheapest_matrix_multiplications(sizes, nr_matrices)
    print(result)


if __name__ == "__main__":
    main()
