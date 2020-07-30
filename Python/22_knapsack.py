import sys

from collections import namedtuple

Item = namedtuple('Item', ['weight', 'value'])


def convert_to_intlist(line):
    return [int(item) for item in line.split()]


def read_items(reader, nr_items):
    items = []
    for _ in range(nr_items):
        items.append(Item(*convert_to_intlist(next(reader))))
    return items


def solve_knapsack_problem(items, nr_items, capacity):
    knapsack = [[-1] * nr_items for _ in range(capacity)]

    def solve(current_capacity, n):
        if current_capacity == 0 or n == 0:
            return 0
        if knapsack[current_capacity-1][n-1] != -1:
           return knapsack[current_capacity-1][n-1]

        weight, value = items[n-1]
        if current_capacity < weight:
            optimal_value = solve(current_capacity, n - 1)
        else:
            optimal_value = max(solve(current_capacity - weight, n - 1) + value, solve(current_capacity, n - 1))
        knapsack[current_capacity-1][n-1] = optimal_value
        return optimal_value

    _ = solve(capacity, nr_items)
    return find_item_indices(knapsack, items, nr_items, capacity)


def find_item_indices(knapsack, items, nr_items, capacity):
    indices = []
    current_capacity = capacity
    n = nr_items
    while n >= 1 and current_capacity > 0:
        weight, _ = items[n-1]
        if current_capacity >= weight:
            previous = 0 if n == 1 else knapsack[current_capacity-1][n-2]
            if knapsack[current_capacity-1][n-1] != previous:
                indices.append(n)
                current_capacity -= weight
        n -= 1
    return sorted(indices)


def main():
    reader = sys.stdin
    nr_items, capacity = convert_to_intlist(next(reader))
    items = read_items(reader, nr_items)
    result = solve_knapsack_problem(items, nr_items, capacity)
    print(len(result))
    print(' '.join([str(elem) for elem in result]))


if __name__ == "__main__":
    main()
