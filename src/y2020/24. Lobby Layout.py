#!/usr/bin/env python3

"""
AoC Day 24 - Lobby Layout - in Python.

Another Game of Life-like implementation.
"""

__author__ = "Lucas Moeskops"
__date__ = "2020-12-24"

from collections import Counter, defaultdict, deque
from functools import partial, reduce
from math import prod
from operator import itemgetter
from sys import stdin

neighbours = [(-1, 1), (1, 1), (1, -1), (-1, -1), (-2, 0), (2, 0)]

vector_add = lambda t, u: tuple(a + b for (a, b) in zip(t, u))

def locate(description):
    if not description:
        return 0, 0
    l = 2 if any(description.startswith(s) for s in ('se', 'sw', 'ne', 'nw')) else 1
    command = description[:l]
    x, y = locate(description[l:])
    if command == 'e':
        return x + 2, y
    if command == 'w':
        return x - 2, y
    if command == 'se':
        return x + 1, y + 1
    if command == 'sw':
        return x - 1, y + 1
    if command == 'ne':
        return x + 1, y - 1
    if command == 'nw':
        return x - 1, y - 1

def animate(floor):
    new_floor = set()
    white_possibilities = set()
    for p in floor:
        count = 0
        for n in neighbours:
            q = vector_add(n, p)
            if q in floor:
                count += 1
            else:
                white_possibilities.add(q)
        if count == 1 or count == 2:
            new_floor.add(p)
    for p in white_possibilities:
        if sum(1 for n in neighbours if vector_add(n, p) in floor) == 2:
            new_floor.add(p)
    return new_floor

lines = stdin.read().split('\n')

floor = set()
for line in lines:
    p = locate(line)
    getattr(floor, 'remove' if p in floor else 'add')(p)

print(f'1: {len(floor)}')

floor = reduce(lambda floor, _: animate(floor), range(100), floor)
print(f'2: {len(floor)}')