#!/usr/bin/env python3

"""
AoC Day 12 - Passage Pathing - in Python.
"""

__author__ = "Lucas Moeskops"
__date__ = "2021-12-12"

from collections import defaultdict
from string import ascii_uppercase
from sys import stdin

from helpers import timed

lines = stdin.read().strip().split('\n')
nodes = [line.split('-') for line in lines]


def neighbours(x, y):
    return (x, y - 1), (x + 1, y), (x, y + 1), (x - 1, y)


def make_map():
    m = defaultdict(list)
    for _from_, to in nodes:
        m[_from_].append(to)
        m[to].append(_from_)
    return m


def is_small(name):
    return not any(x in ascii_uppercase for x in name)


def visit_rec(m, p, can_visit_small_cave_twice):
    if p[-1] == 'end':
        yield p
        return
    for option in m[p[-1]]:
        can_visit_small_cave_twice_still = can_visit_small_cave_twice
        if is_small(option) and option in p:
            if can_visit_small_cave_twice and option != 'start':
                can_visit_small_cave_twice_still = False
            else:
                continue

        r = p[:]
        r.append(option)
        yield from visit_rec(m, r, can_visit_small_cave_twice_still)


def visit_all(m, allow_cave_twice=False):
    yield from visit_rec(m, ['start'], can_visit_small_cave_twice=allow_cave_twice)


@timed
def task_1():
    return sum(1 for _ in visit_all(make_map(), allow_cave_twice=False))


@timed
def task_2():
    return sum(1 for _ in visit_all(make_map(), allow_cave_twice=True))


print(f'[Part 1]: {task_1()}')
print(f'[Part 2]: {task_2()}')
