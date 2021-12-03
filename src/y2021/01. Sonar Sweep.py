#!/usr/bin/env python3

"""
AoC Day 1 - Sonar Sweep - in Python.
"""

__author__ = "Lucas Moeskops"
__date__ = "2021-12-01"

from itertools import islice, pairwise, tee
from sys import stdin

from helpers import timed

lines = stdin.read().split('\n')
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


@timed
def task_1():
    return sum(a < b for a, b in pairwise(ints))


@timed
def task_2():
    window = sliding_sum(3)
    return sum(a < b for a, b in pairwise(window(ints)))


@timed
def task_2_cp():
    """ Making use of a + b + c < b + c + d === a < d """
    return sum(a < b for a, b in zip(ints, ints[3:]))


print(f'[Part 1] [Comparing pairs]: {task_1()}')
print(f'[Part 2] [Sliding window]: {task_2()}')
print(f'[Part 2] [Comparing pairs]: {task_2_cp()}')
