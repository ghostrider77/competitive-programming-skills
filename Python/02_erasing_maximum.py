import sys


def convert_to_intlist(line):
    return [int(elem) for elem in line.split()]


def find_argmax(array, m, count):
    max_indices = [ix for ix, item in enumerate(array) if item == m]
    if count >= 3:
        return max_indices[2]
    return max_indices[0]


def remove_maximum(array):
    m = max(array)
    count = sum(1 for item in array if item == m)
    max_ix = find_argmax(array, m, count)
    return [item for ix, item in enumerate(array) if ix != max_ix]


def main():
    reader = sys.stdin
    _ = next(reader)
    array = convert_to_intlist(next(reader))
    result = remove_maximum(array)
    print(' '.join([str(item) for item in result]))



if __name__ == '__main__':
    main()
