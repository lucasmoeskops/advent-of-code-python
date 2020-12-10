from collections import Counter
from itertools import groupby
from math import prod
from sys import stdin

count = lambda i: 1 + (i - 1) * i // 2

lines = stdin.read().split('\n')
ns = [0, *(sn := sorted(map(int, lines))), sn[-1] + 3]
diffs = [j - i for i, j in zip(ns[:-1], ns[1:])]

print(f'1: {prod(Counter(diffs).values())}')
print(f'2: {prod(count(len([*g])) for k, g in groupby(diffs) if k == 1)}')
