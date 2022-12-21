from collections import deque
from sys import stdin

DATA = stdin.read().split('\n')

ns = [int(x) for x in DATA]
length = len(ns)
mod_length = length - 1
target = ns.index(0)

for multiplier, num_rounds in ((1, 1), (811589153, 10)):
    sequence = [n * multiplier for n in ns]
    numbers = deque(range(length))

    for _ in range(num_rounds):
        for i in range(length):
            old = numbers.index(i)
            new = sequence[i] % mod_length
            numbers.rotate(-old)
            numbers.popleft()
            numbers.rotate(-new)
            numbers.appendleft(i)

    start = numbers.index(target)
    print(sum(sequence[numbers[(start + i) % len(numbers)]] for i in (1000, 2000, 3000)))
