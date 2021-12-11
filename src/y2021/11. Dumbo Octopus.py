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
bounds = [0, len(lines[0]), len(lines), 0]


def neighbours_diagonal(x, y):
    return (
        (x, y - 1),
        (x + 1, y - 1),
        (x + 1, y),
        (x + 1, y + 1),
        (x, y + 1),
        (x - 1, y + 1),
        (x - 1, y),
        (x - 1, y - 1),
    )


def step(_octopuses_):
    updates = {p: v + 1 for p, v in _octopuses_.items()}
    done = set()
    recheck = True
    first = True
    while recheck:
        recheck = False
        for p, v in updates.items():
            if v > 9 and p not in done:
                done.add(p)
                recheck = True
                for n in neighbours_diagonal(*p):
                    if n in _octopuses_:
                        updates[n] = updates[n] + 1
        if first and len(done) == bounds[2] * bounds[1]:
            return -1
        first = False
    updates.update({p: 0 for p in done})
    _octopuses_.update(updates)
    return len(done)


@timed
def task_1():
    _octopuses_ = {k: v for k, v in octopuses.items()}
    return sum(step(_octopuses_) for _ in range(100))


@timed
def task_2():
    _octopuses_ = {k: v for k, v in octopuses.items()}
    return next(dropwhile(lambda _: step(_octopuses_) >= 0, count(1)))


print(f'[Part 1]: {task_1()}')
print(f'[Part 2]: {task_2()}')
