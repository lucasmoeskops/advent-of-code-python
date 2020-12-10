from sys import stdin
from itertools import combinations

def find_invalid(numbers, num=25):
    options = range(num, len(numbers))
    test = lambda i: numbers[i] not in map(sum, combinations(numbers[i - num:i], 2))
    yield from map(numbers.__getitem__, filter(test, options))

def find_weakness(numbers, number):
    if len(numbers) < 2:
        return
    i, j, total = 0, 1, numbers[0] + numbers[1]
    while j < len(numbers) - 1:
        if total == number:
            return min(numbers[i:j+1]) + max(numbers[i:j+1])
        if total < number:
            j += 1
            total += numbers[j]
        else:
            total -= numbers[i]
            i += 1

lines = stdin.read().split('\n')
numbers = [int(line) for line in lines]

first_invalid = next(find_invalid(numbers))

print(f'1: {first_invalid}')
print(f'2: {find_weakness(numbers, first_invalid)}')
