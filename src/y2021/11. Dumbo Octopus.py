#!/usr/bin/env python3

"""
AoC Day 11 - Dumbo Octopus - in Python.
"""

__author__ = "Lucas Moeskops"
__date__ = "2021-12-11"

from itertools import count, dropwhile
from sys import stdin

from helpers import timed

lines = stdin.read().split('\n')
octopuses = {(x, y): int(v) for y, line in enumerate(lines) for x, v in enumerate(line)}
width, height = (len(lines[0]), len(lines))


def neighbours_diagonal(x, y):
    for d in range(-1, 2):
        for e in range(-1, 2):
            if d or e:
                yield x + d, y + e


def step(_octopuses_):
    recheck = True
    flashed = 0
    for p, v in _octopuses_.items():
        _octopuses_[p] = v + 1
    while recheck:
        recheck = False
        for p, v in _octopuses_.items():
            if v > 9:
                flashed += 1
                _octopuses_[p] = 0
                recheck = True
                for (x, y) in neighbours_diagonal(*p):
                    if 0 <= x < width and 0 <= y < height:
                        if nv := _octopuses_[(x, y)]:
                            _octopuses_[(x, y)] = nv + 1
    return flashed


@timed
def task_1():
    _octopuses_ = octopuses.copy()
    return sum(step(_octopuses_) for _ in range(100))


@timed
def task_2():
    _octopuses_, num = octopuses.copy(), width * height
    return next(dropwhile(lambda _: step(_octopuses_) != num, count(1)))


print(f'[Part 1]: {task_1()}')
print(f'[Part 2]: {task_2()}')
