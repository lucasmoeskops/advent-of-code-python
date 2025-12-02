#!/usr/bin/env python3

"""
AoC Day 23 - LAN Party - in Python.
"""

__author__ = "Lucas Moeskops"
__date__ = "2024-12-23"
__summary__ = "Back to year 2016, day 9"

from lib import *


puzzle = read_input(2024, 23)
lookup = defaultdict(set)
for line in puzzle.split('\n'):
    a, b = line.split('-')
    lookup[a].add(b)
    lookup[b].add(a)

triplets = []
for a in lookup:
    for b in lookup[a]:
        for c in lookup[b]:
            if c in lookup[a]:
                if a < b < c:
                    triplets.append((a, b, c))

sets = []
seen = set()
queue = deque(lookup.keys())
best = ''
best_size = 0

while queue:
    a = queue.popleft()
    joined = [a]

    for b in queue:
        for x in joined:
            if x not in lookup[b]:
                break
        else:
            joined.append(b)

    if len(joined) > best_size:
        best_size = len(joined)
        best = ','.join(map(str, sorted(joined)))

print(sum(1 for s in triplets if any(x.startswith('t') for x in s)))  # Part 1
print(best)  # Part 2