#!/usr/bin/env python3

"""
AoC Day 5 - Hydrothermal Venture - in Python.
"""

__author__ = "Lucas Moeskops"
__date__ = "2021-12-05"


from collections import Counter
from itertools import zip_longest

from sys import stdin


LINES = stdin.read().split('\n')
vents = [tuple(map(int, line.replace(' -> ', ',').split(','))) for line in LINES]


def part_1():
    field = Counter()
    for x1, y1, x2, y2 in vents:
        if x1 == x2:
            _y1_, _y2_ = sorted([y1, y2])
            for y in range(_y1_, _y2_ + 1):
                field[(x1, y)] += 1
        elif y1 == y2:
            _x1_, _x2_ = sorted([x1, x2])
            for x in range(_x1_, _x2_ + 1):
                field[(x, y1)] += 1
    return sum(1 for p in field.values() if p >= 2)


def part_2():
    field = Counter()
    for x1, y1, x2, y2 in vents:
        _x1_, _x2_ = sorted([x1, x2])
        _y1_, _y2_ = sorted([y1, y2])
        rx = range(_x1_, _x2_ + 1) if x2 > x1 else reversed(range(_x1_, _x2_ + 1))
        ry = range(_y1_, _y2_ + 1) if y2 > y1 else reversed(range(_y1_, _y2_ + 1))
        fill_value = x1 if x1 == x2 else y1
        for x, y in zip_longest(rx, ry, fillvalue=fill_value):
            field[(x, y)] += 1
    return sum(1 for p in field.values() if p >= 2)


print(part_1())
print(part_2())
