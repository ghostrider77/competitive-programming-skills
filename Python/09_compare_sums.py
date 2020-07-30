import sys


def convert_to_floatlist(line):
    return [float(elem) for elem in line.split()]


def compare(arr1, arr2):
    s1 = sum(arr1)
    s2 = sum(arr2)
    if abs(s1 - s2) < 1e-6:
        return 'SUM(A)=SUM(B)'
    if s1 < s2:
        return 'SUM(A)<SUM(B)'
    return 'SUM(A)>SUM(B)'


def main():
    reader = sys.stdin
    _ = next(reader)
    arr1 = convert_to_floatlist(next(reader))
    arr2 = convert_to_floatlist(next(reader))
    result = compare(arr1, arr2)
    print(result)


if __name__ == '__main__':
    main()
