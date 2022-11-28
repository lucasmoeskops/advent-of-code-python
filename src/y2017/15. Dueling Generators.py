from collections import deque, Counter
from functools import reduce
from itertools import count
from operator import itemgetter
from sys import stdin


DESCRIPTIONS = stdin.read().split('\n')
START_A, START_B = [int(''.join(c for c in description if '0' <= c <= '9')) for description in DESCRIPTIONS]


def make_generator(start_value, multiplier, is_multiple_of=1):
    value = start_value
    while True:
        value *= multiplier
        value %= 2147483647
        if value % is_multiple_of:
            continue
        yield value


generator_a = make_generator(START_A, 16807)
generator_b = make_generator(START_B, 48271)

print(sum(1 for i in range(40 * 1000 * 1000) if next(generator_a) % 65536 == next(generator_b) % 65536))

generator_a = make_generator(START_A, 16807, 4)
generator_b = make_generator(START_B, 48271, 8)

print(sum(1 for i in range(5 * 1000 * 1000) if next(generator_a) % 65536 == next(generator_b) % 65536))
