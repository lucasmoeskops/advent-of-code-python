from sys import stdin
from collections import Counter
from itertools import count, takewhile

def find_diffs_product(sorted_numbers):
    diffs = Counter()
    p = 0
    for x in sorted_numbers:
        diffs[x - p] += 1
        p = x
    return diffs[1] * diffs[3]

def find_total_arrangements(sorted_numbers):
    find_options = (lambda i:
        (lambda n, rest: takewhile(lambda m: sorted_numbers[m] - n <= 3, rest)
        )(sorted_numbers[i], range(i+1, l)))
    l = len(sorted_numbers)
    tots = {l - 1: 1}
    for i in reversed(range(0, l - 1)):
        tots[i] = sum(map(tots.__getitem__, find_options(i)))
    return tots[0]

lines = stdin.read().split('\n')
numbers = [int(line) for line in lines]

sorted_numbers = sorted(numbers)

adapter_input = 0
adapter_output = sorted_numbers[-1] + 3
all_numbers = [adapter_input] + sorted_numbers + [adapter_output]

print(f'1: {find_diffs_product(all_numbers)}')
print(f'2: {find_total_arrangements(all_numbers)}')
