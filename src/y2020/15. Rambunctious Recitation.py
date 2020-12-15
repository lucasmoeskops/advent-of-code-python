from itertools import count
from sys import stdin

numbers = [int(line) for line in stdin.read().split(',')]

def play(numbers, return_index):
    memory = [0] * return_index
    for i, n in enumerate(numbers[:-1], start=1):
        memory[n] = i
    last_spoken = numbers[-1]
    for i in range(len(numbers), return_index):
        new = i - memory[last_spoken] if memory[last_spoken] else 0
        memory[last_spoken] = i
        last_spoken = new
    return last_spoken

print(f'1: {play(numbers, 2020)}')
print(f'2: {play(numbers, int(3e7))}')