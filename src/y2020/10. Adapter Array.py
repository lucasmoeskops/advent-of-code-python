from collections import Counter
from itertools import groupby
from math import prod
from sys import stdin

diffs = lambda ns: map(lambda i: ns[i] - ns[i - 1], range(1, len(ns)))
count = lambda i: 1 + (i - 1) * i // 2

lines = stdin.read().split('\n')
ns = [0, *(sn := sorted(map(int, lines))), sn[-1] + 3]

print(f'1: {prod(Counter(diffs(ns)).values())}')
print(f'2: {prod(count(len([*g])) for k, g in groupby(diffs(ns)) if k == 1)}')
