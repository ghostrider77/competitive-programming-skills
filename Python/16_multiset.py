import sys


def convert_to_intlist(line):
    return [int(elem) for elem in line.split()]


def read_intervals(reader, n):
    intervals = []
    for _ in range(n):
        line = next(reader)
        intervals.append(tuple(convert_to_intlist(line)))
    return intervals


def calc_occurrences(intervals, n):
    smallest, _ = min(intervals, key=lambda x: x[0])
    _, largest = max(intervals, key=lambda x: x[1])
    size = largest - smallest + 1
    started = [0] * size
    finished = [0] * size
    counts = [0] * size

    for a, b in intervals:
        started[a-smallest] += 1
        finished[b-smallest] += 1

    counts[0] = started[0]
    for ix in range(1, size):
        counts[ix] = counts[ix-1] + started[ix] - finished[ix-1]

    return ((ix + smallest, count) for ix, count in enumerate(counts) if count != 0)


def main():
    reader = sys.stdin
    n = int(next(reader))
    intervals = read_intervals(reader, n)
    result = calc_occurrences(intervals, n)
    for number, count in result:
        print('{} {}'.format(number, count))


if __name__ == '__main__':
    main()
