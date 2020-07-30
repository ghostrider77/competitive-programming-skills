import sys

from collections import namedtuple

Point = namedtuple('Point', ['x', 'ix'])


def read_points(reader, n):
    points = []
    for ix in range(n):
        x = int(next(reader))
        points.append(Point(x, ix + 1))
    return points


def max_distances(points):
    first_point = points[0]
    left = first_point
    right = first_point
    result = [(left.ix, right.ix)]
    for p in points[1:]:
        if p.x < left.x:
            left = p
        if p.x > right.x:
            right = p
        result.append((left.ix, right.ix))
    return result


def main():
    reader = sys.stdin
    n = int(next(reader))
    xs = read_points(reader, n)
    result = max_distances(xs)
    for points in result:
        print(' '.join([str(x) for x in points]))


if __name__ == '__main__':
    main()
