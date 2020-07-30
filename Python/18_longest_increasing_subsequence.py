import sys

INFINITY = 1000001


def convert_to_intlist(line):
    return [int(elem) for elem in line.split()]


def put_number_to_a_stack(number, stack_list):
    for stack in stack_list:
        if number <= stack[-1]:
            stack.append(number)
            return
    stack_list.append([number])


def calc_longest_increasing_subsequence(array):
    stack_list = [[INFINITY]]
    for number in array:
        put_number_to_a_stack(number, stack_list)
    return len(stack_list)


def main():
    reader = sys.stdin
    _ = int(next(reader))
    array = convert_to_intlist(next(reader))
    result = calc_longest_increasing_subsequence(array)
    print(result)


if __name__ == '__main__':
    main()
