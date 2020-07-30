import sys

from collections import namedtuple

Item = namedtuple('Item', ['weight', 'value'])


def convert_to_intlist(line):
    return [int(elem) for elem in line.split()]


def read_items(reader, n):
    items = []
    for _ in range(n):
        line = next(reader)
        items.append(Item(*convert_to_intlist(line)))
    return items


def split_items(items):
    smallest_items = []
    rest = []
    for item in items:
        if item.weight == 1:
            smallest_items.append(item)
        else:
            rest.append(item)
    return smallest_items, rest


def merge_smallest_items(items):
    if len(items) % 2 == 1:
        items.append(Item(weight=1, value=0))

    sorted_items = sorted(items, key=lambda x: x.value, reverse=True)
    size = len(sorted_items) // 2
    merged = []
    for k in range(size):
        _, value1 = sorted_items[2*k]
        _, value2 = sorted_items[2*k+1]
        merged.append(Item(2, value1 + value2))
    return merged


def find_item_with_largest_value(items):
    sorted_items = sorted(items, key=lambda x: x.value, reverse=True)
    first = sorted_items[0]
    return first.value, sorted_items[1:]


def solve_knapsack_problem(items, capacity):
    total_cost = 0
    while capacity > 0:
        smallest_items, rest = split_items(items)
        merged_items = []
        if capacity % 2 == 0:
            merged_items = merge_smallest_items(smallest_items)
        else:
            capacity -= 1
            if smallest_items:
                value, remaining_items = find_item_with_largest_value(smallest_items)
                total_cost += value
                merged_items = merge_smallest_items(remaining_items)

        capacity = capacity // 2
        items = [Item(weight=item.weight//2, value=item.value) for item in rest + merged_items]
    return total_cost


def main():
    reader = sys.stdin
    n, capacity = convert_to_intlist(next(reader))
    items = read_items(reader, n)
    result = solve_knapsack_problem(items, capacity)
    print(result)


if __name__ == '__main__':
    main()
