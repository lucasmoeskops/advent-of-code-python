#!/usr/bin/env python3

"""
AoC Day 7 - The Treachery of Whales - in Python.
"""

__author__ = "Lucas Moeskops"
__date__ = "2021-12-07"

from sys import stdin

from helpers import timed

positions = [int(v) for v in stdin.read().split(',')]


def calc_best(f, s):
    base = f(s)
    less, prev = s - 1, base
    more, nxt = s + 1, base
    while (new := f(less)) < prev:
        less -= 1
        prev = new
    while (new := f(more)) < nxt:
        more += 1
        nxt = new
    return min(base, prev, nxt)


@timed
def task_1():
    return calc_best(
        lambda t: sum(abs(s - t) for s in positions),
        sorted(positions)[len(positions) // 2],
    )


@timed
def task_2():
    return calc_best(
        lambda t: sum(abs(s - t) * (abs(s - t) + 1) // 2 for s in positions),
        int(sum(positions) / len(positions)),
    )


print(f'[Part 1]: {task_1()}')
print(f'[Part 2]: {task_2()}')
