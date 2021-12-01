#!/usr/bin/env python3

"""
AoC Day 1 - Sonar Sweep - in Python.
"""

__author__ = "Lucas Moeskops"
__date__ = "2021-12-01"

from itertools import islice, pairwise, tee
from sys import stdin


full = stdin.read()
lines = full.split('\n')
ints = [int(line) for line in lines if line]


def sliding_sum(size):
    def iterate(iterable):
        it1, it2 = tee(iter(iterable), 2)
        s = sum(islice(it1, size))
        yield s
        for v in it1:
            s -= next(it2)
            s += v
            yield s
    return iterate


def task_1():
    return sum(a < b for a, b in pairwise(ints))


def task_2():
    summer = sliding_sum(3)
    return sum(a < b for a, b in pairwise(summer(ints)))


print(f'1: {task_1()}')
print(f'2: {task_2()}')
