import sys


def convert_to_intlist(line):
    return tuple(int(item) for item in line.split())


def longest_common_subsequence(s1, s2, n):
    longest_path = [[0] * (n + 1) for _ in range(n+1)]
    backtrack =  [[0] * n for _ in range(n)]
    for ix in range(n):
        for jx in range(n):
            longest_path[ix+1][jx+1] = max(longest_path[ix][jx+1],
                                           longest_path[ix+1][jx],
                                           longest_path[ix][jx] + int(s1[ix] == s2[jx]))
            if longest_path[ix+1][jx+1] == longest_path[ix][jx+1]:
                backtrack[ix][jx] = -1
            elif longest_path[ix+1][jx+1] == longest_path[ix+1][jx]:
                backtrack[ix][jx] = 1
            elif (longest_path[ix+1][jx+1] == longest_path[ix][jx] + 1) and s1[ix] == s2[jx]:
                backtrack[ix][jx] = 0
    return output_longest_common_subsequence(backtrack, n)


def output_longest_common_subsequence(backtrack, n):
    ixs1 = []
    ixs2 = []
    ix, jy = n, n
    while ix > 0 and jy > 0:
        if backtrack[ix-1][jy-1] == 0:
            ixs1.append(ix-1)
            ixs2.append(jy-1)
            ix -= 1
            jy -= 1
        elif backtrack[ix-1][jy-1] == -1:
            ix -= 1
        else:
            jy -= 1
    return ixs1[::-1], ixs2[::-1]


def stringify_array(array):
    return ' '.join([str(item) for item in array])


def main():
    reader = sys.stdin
    n = int(next(reader))
    s1 = convert_to_intlist(next(reader))
    s2 = convert_to_intlist(next(reader))
    ixs1, ixs2 = longest_common_subsequence(s1, s2, n)
    print(len(ixs1))
    print(stringify_array(ixs1))
    print(stringify_array(ixs2))


if __name__ == "__main__":
    main()
