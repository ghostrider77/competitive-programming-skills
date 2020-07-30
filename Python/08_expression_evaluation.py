import sys
import operator


OPERATIONS = {'+': operator.add, '-': operator.sub}


def split_string(string):
    numbers = []
    operations = []
    current_number = []
    for char in string:
        if char == '+' or char == '-':
            number = int(''.join(current_number))
            current_number = []
            numbers.append(number)
            operations.append(char)
        else:
            current_number.append(char)
    number = int(''.join(current_number))
    numbers.append(number)
    return numbers, operations


def calculate(numbers, operations):
    result = numbers[0]
    for n, op in zip(numbers[1:], operations):
        result = OPERATIONS[op](result, n)
    return result


def main():
    reader = sys.stdin
    string = next(reader).rstrip()
    numbers, operations = split_string(string)
    result = calculate(numbers, operations)
    print(result)


if __name__ == '__main__':
    main()
