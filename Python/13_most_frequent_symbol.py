import sys

from math import ceil, log2


ALPHABET = 'abcdefghijklmnopqrstuvwxyz'
SIZE = len(ALPHABET)


def convert_to_intlist(line):
    return [int(elem) for elem in line.split()]


def read_queries(reader, n):
    queries = []
    for _ in range(n):
        a, b = convert_to_intlist(next(reader))
        queries.append((a, b))
    return queries


def find_character(frequencies):
    char, _ = max(zip(ALPHABET, frequencies), key=lambda x: x[1])
    return char


def combine(frequencies1, frequencies2):
    return tuple(f1 + f2 for f1, f2 in zip(frequencies1, frequencies2))


def create_single_letter_frequency_list(char):
    return tuple(1 if letter == char else 0 for letter in ALPHABET)


def calc_segment_tree(array, n):
    max_nr_nodes = 2 ** (ceil(log2(n)) + 1) - 1
    default_frequencies = (0,) * SIZE
    segment_tree = [default_frequencies] * max_nr_nodes

    def build_tree(node_ix, left_end, right_end):
        if left_end == right_end:
            segment_tree[node_ix] = create_single_letter_frequency_list(array[left_end])
        else:
            left_child_ix = 2 * node_ix + 1
            right_child_ix = left_child_ix + 1
            middle_ix = (left_end + right_end) // 2
            build_tree(left_child_ix, left_end, middle_ix)
            build_tree(right_child_ix, middle_ix + 1, right_end)
            segment_tree[node_ix] = combine(segment_tree[left_child_ix], segment_tree[right_child_ix])

    build_tree(0, 0, n - 1)
    return segment_tree


def most_frequent_chars_in_range(array, queries):
    n = len(array)
    segment_tree = calc_segment_tree(array, n)

    def get_most_frequent_char_in_segment(node_ix, left_end, right_end, i, j):
        if i <= left_end and j >= right_end:
            return segment_tree[node_ix]
        if i > right_end or j < left_end:
            return (0,) * SIZE
        middle_ix = (left_end + right_end) // 2
        frequencies1 = get_most_frequent_char_in_segment(2*node_ix+1, left_end, middle_ix, i, j)
        frequencies2 = get_most_frequent_char_in_segment(2*node_ix+2, middle_ix + 1, right_end, i, j)
        return combine(frequencies1, frequencies2)

    result = []
    for a, b in queries:
        freqs = get_most_frequent_char_in_segment(0, 0, n - 1, a - 1, b - 1)
        result.append(find_character(freqs))
    return result


def main():
    reader = sys.stdin
    array = list(next(reader).rstrip())
    nr_queries = int(next(reader))
    queries = read_queries(reader, nr_queries)
    result = most_frequent_chars_in_range(array, queries)
    for char in result:
        print(char)


if __name__ == '__main__':
    main()
