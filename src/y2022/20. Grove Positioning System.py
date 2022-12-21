from collections import deque
from sys import stdin

DATA = stdin.read().split('\n')

ns = [int(x) for x in DATA]
length = len(ns)
target = ns.index(0), 0

for multiplier, num_rounds in ((1, 1), (811589153, 10)):
    sequence = [n * multiplier for n in ns]
    numbers = deque(enumerate(sequence))

    for _ in range(num_rounds):
        for tuple in zip(range(length), sequence):
            old = numbers.index(tuple)
            new = (old + tuple[1]) % (length - 1)
            numbers.remove(tuple)
            numbers.insert(new, tuple)

    start = numbers.index(target)
    print(sum(numbers[(start + i) % len(numbers)][1] for i in (1000, 2000, 3000)))
