#!/usr/bin/env python3

"""
AoC Day 9 - Smoke Basin - in Python.
"""

__author__ = "Lucas Moeskops"
__date__ = "2021-12-09"

from collections import Counter
from functools import cache

from math import prod
from sys import stdin

from helpers import timed

lines = stdin.read().split('\n')
coords = {(x, y): int(v) for y, line in enumerate(lines) for x, v in enumerate(line)}


def neighbours(x, y):
    return (x, y - 1), (x + 1, y), (x, y + 1), (x - 1, y)


@timed
def task_1():
    return sum(
        h + 1
        for p, h in coords.items()
        if all(coords.get(q, 10) > h for q in neighbours(*p))
    )


@cache
def find_low(p):
    h = coords[p]
    for q in neighbours(*p):
        if coords.get(q, 10) < h:
            return find_low(q)
    return p


@timed
def task_2():
    participants = (p for p, h in coords.items() if h < 9)
    basins = Counter(map(find_low, participants))
    return prod(s for _, s in basins.most_common(3))


print(f'[Part 1]: {task_1()}')
print(f'[Part 2]: {task_2()}')
