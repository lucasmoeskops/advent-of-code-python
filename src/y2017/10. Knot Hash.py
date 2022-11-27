from collections import defaultdict, deque
from functools import reduce
from sys import stdin


NUM_ELEMENTS = 256


def run(rounds, lengths):
    deque_ = deque(range(NUM_ELEMENTS))
    skip_size = 0
    total_skipped = 0
    for _ in range(rounds):
        for length in lengths:
            deque_.extendleft([deque_.popleft() for _ in range(length)])
            deque_.rotate(-length - skip_size)
            total_skipped += length + skip_size
            skip_size += 1
    deque_.rotate(total_skipped)
    return deque_


data = stdin.read()
lengths = [int(value.strip(' ')) for value in data.split(',')]

numbers = run(1, lengths)
print(numbers[0] * numbers[1])

numbers = run(64, [ord(c) for c in data] + [17, 31, 73, 47, 23])
print(''.join(hex(reduce(int.__xor__, (numbers.popleft() for _ in range(16))))[2:] for _ in range(16)))
