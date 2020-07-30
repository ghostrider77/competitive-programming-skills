import sys


def increment_length(digits):
    length = len(digits)
    if all(d == '9' for d in digits):
        return length + 1
    return length


def main():
    reader = sys.stdin
    digits = next(reader).rstrip()
    result = increment_length(digits)
    print(result)


if __name__ == '__main__':
    main()
