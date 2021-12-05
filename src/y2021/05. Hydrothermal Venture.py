#!/usr/bin/env python3

"""
AoC Day 5 - Hydrothermal Venture - in Python.
"""

__author__ = "Lucas Moeskops"
__date__ = "2021-12-05"

from collections import Counter
from itertools import zip_longest
from operator import itemgetter

from sys import stdin

from helpers import timed, parse_from_re

full = stdin.read()
lines = full.split('\n')
parser_re = r'(?P<x1>\d+),(?P<y1>\d+) -> (?P<x2>\w+),(?P<y2>\d+)'
parser_map = {'x1': int, 'x2': int, 'y1': int, 'y2': int}
vents = list(parse_from_re(parser_re, parser_map, lines))


@timed
def task_1():
    field = Counter()
    getter = itemgetter('x1', 'y1', 'x2', 'y2')
    for x1, y1, x2, y2 in map(getter, vents):
        if x1 == x2:
            _y1_, _y2_ = sorted([y1, y2])
            for y in range(_y1_, _y2_ + 1):
                field[(x1, y)] += 1
        elif y1 == y2:
            _x1_, _x2_ = sorted([x1, x2])
            for x in range(_x1_, _x2_ + 1):
                field[(x, y1)] += 1
    return len([p for p in field.values() if p >= 2])


@timed
def task_2():
    field = Counter()
    getter = itemgetter('x1', 'y1', 'x2', 'y2')
    for x1, y1, x2, y2 in map(getter, vents):
        _x1_, _x2_ = sorted([x1, x2])
        _y1_, _y2_ = sorted([y1, y2])
        rx = range(_x1_, _x2_ + 1) if x2 > x1 else reversed(range(_x1_, _x2_ + 1))
        ry = range(_y1_, _y2_ + 1) if y2 > y1 else reversed(range(_y1_, _y2_ + 1))
        fill_value = x1 if x1 == x2 else y1
        for x, y in zip_longest(rx, ry, fillvalue=fill_value):
            field[(x, y)] += 1
    return len([p for p in field.values() if p >= 2])


print(f'[Part 1]: {task_1()}')
print(f'[Part 2]: {task_2()}')
