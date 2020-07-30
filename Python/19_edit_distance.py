import sys

from collections import namedtuple

Penalties = namedtuple('Penalties', ['insertion', 'deletion', 'substitution'])


def convert_to_intlist(line):
    return [int(elem) for elem in line.split()]


def calc_edit_distance(string_1, string_2, n, m, penalties):
    edit_distance = [[0] * (m+1) for _ in range(n+1)]
    for ix in range(1, n+1):
        edit_distance[ix][0] = ix * penalties.deletion
    for jy in range(1, m+1):
        edit_distance[0][jy] = jy * penalties.insertion

    for ix in range(n):
        for jy in range(m):
            deletion = edit_distance[ix][jy+1] + penalties.deletion
            insertion = edit_distance[ix+1][jy] + penalties.insertion
            match = edit_distance[ix][jy]
            if string_1[ix] != string_2[jy]:
                match += penalties.substitution

            edit_distance[ix+1][jy+1] = min(insertion, deletion, match)
    return edit_distance[-1][-1]


def main():
    reader = sys.stdin
    n, m = convert_to_intlist(next(reader))
    s1 = next(reader).rstrip()
    s2 = next(reader).rstrip()
    penalties = Penalties(*convert_to_intlist(next(reader)))
    dist = calc_edit_distance(s1, s2, n, m, penalties)
    print(dist)


if __name__ == "__main__":
    main()
