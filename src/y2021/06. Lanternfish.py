#!/usr/bin/env python3

"""
AoC Day 6 - Lanternfish - in Python.
"""

__author__ = "Lucas Moeskops"
__date__ = "2021-12-06"


from collections import Counter, deque
from sys import stdin


INITIAL = [int(v) for v in stdin.read().split(',')]


def find_population(num_days):
    counter = Counter(INITIAL)
    per_day = deque([counter[i] for i in range(9)])
    for i in range(num_days):
        at_zero = per_day.popleft()
        per_day[-2] += at_zero
        per_day.append(at_zero)
    return sum(per_day)


def part_1():
    return find_population(80)


def part_2():
    return find_population(256)


print(part_1())
print(part_2())
