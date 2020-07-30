import sys

from collections import namedtuple

Point = namedtuple('Point', ['x', 'y', 'ix'])
Value = namedtuple('Value', ['value', 'ix'])


def convert_to_intlist(line):
    return [int(elem) for elem in line.split()]


def read_points(reader, n):
    points = []
    for ix in range(n):
        x, y = convert_to_intlist(next(reader))
        points.append(Point(x, y, ix + 1))
    return points


def max_manhattan_distances(points):
    p0 = points[0]
    max_sum = Value(p0.x + p0.y, p0.ix)
    max_diff = Value(p0.x - p0.y, p0.ix)
    min_sum = Value(p0.x + p0.y, p0.ix)
    min_diff = Value(p0.x - p0.y, p0.ix)

    result = [(p0.ix, p0.ix)]
    for p in points[1:]:
        if p.x + p.y > max_sum.value:
            max_sum = Value(p.x + p.y, p.ix)
        if p.x + p.y < min_sum.value:
            min_sum = Value(p.x + p.y, p.ix)
        if p.x - p.y > max_diff.value:
            max_diff = Value(p.x - p.y, p.ix)
        if p.x - p.y < min_diff.value:
            min_diff = Value(p.x - p.y, p.ix)

        if (max_sum.value - min_sum.value > max_diff.value - min_diff.value):
            result.append((max_sum.ix, min_sum.ix))
        else:
            result.append((max_diff.ix, min_diff.ix))
    return result


def main():
    reader = sys.stdin
    n = int(next(reader))
    points = read_points(reader, n)
    result = max_manhattan_distances(points)
    for ix_pairs in result:
        print(' '.join([str(x) for x in ix_pairs]))


if __name__ == '__main__':
    main()
