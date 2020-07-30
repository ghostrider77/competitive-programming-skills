import sys


def convert_to_intlist(line):
    return [int(elem) for elem in line.split()]


def calc_prefixes_and_suffices(array, n):
    prefix_sums = [0] * (n + 1)
    suffix_sums = [0] * (n + 1)
    min_prefix_sum = [0] * (n + 1)
    min_suffix_sum = [0] * (n + 1)

    min_prefix_sum[0] = prefix_sums[0]
    for ix in range(1, n + 1):
        prefix_sums[ix] = prefix_sums[ix-1] + array[ix-1]
        min_prefix_sum[ix] = min(min_prefix_sum[ix-1], prefix_sums[ix])

    min_suffix_sum[n] = suffix_sums[n]
    for ix in range(1, n + 1):
        suffix_sums[n-ix] = suffix_sums[n-ix+1] + array[n-ix]
        min_suffix_sum[n-ix] = min(min_suffix_sum[n-ix+1], suffix_sums[n-ix])

    return min_prefix_sum, min_suffix_sum


def calc_maximal_sum_subarrays(array, n):
    min_prefix_sum, min_suffix_sum = calc_prefixes_and_suffices(array, n)
    s = sum(array)
    max_subarray_sum = []
    for ix in range(n):
        if ix == 0:
            res = s - min_suffix_sum[1]
        elif ix == n - 1:
            res = s - min_prefix_sum[n-1]
        else:
            res = s - (min_prefix_sum[ix] + min_suffix_sum[ix+1])
        max_subarray_sum.append(res)
    return max_subarray_sum



def main():
    reader = sys.stdin
    n = int(next(reader))
    array = convert_to_intlist(next(reader))
    result = calc_maximal_sum_subarrays(array, n)
    print(' '.join([str(elem) for elem in result]))


if __name__ == '__main__':
    main()
