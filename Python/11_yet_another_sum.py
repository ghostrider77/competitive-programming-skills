import sys


def convert_to_intlist(line):
    return [int(elem) for elem in line.split()]


def calculate_sum(array):
    s1 = sum(array)
    reciprocals = [1 / elem for elem in array]
    positives = sorted(filter(lambda x: x > 0, reciprocals), key=abs)
    negatives = sorted(filter(lambda x: x < 0, reciprocals), key=abs)
    return (sum(positives) + sum(negatives)) + s1


def main():
    reader = sys.stdin
    _ = next(reader)
    array = convert_to_intlist(next(reader))
    result = calculate_sum(array)
    print(result)


if __name__ == '__main__':
    main()
