#!/usr/bin/env python3

"""
AoC Day 3 - Binary Diagnostic - in Python.
"""

__author__ = "Lucas Moeskops"
__date__ = "2021-12-03"

from functools import reduce
from collections import Counter
from operator import itemgetter

from sys import stdin

from helpers import timed

full = stdin.read()
lines = full.split('\n')


def str2bin(s):
    return int(''.join(s), 2)


@timed
def task_1():
    gamma_rate = [Counter(p).most_common(1)[0][0] for p in zip(*lines)]
    epsilon_rate = [Counter(p).most_common(2)[-1][0] for p in zip(*lines)]
    return str2bin(gamma_rate) * str2bin(epsilon_rate)


def build_reducer(_type_, on_equal):
    def reducer(candidates, i):
        counts = Counter(map(itemgetter(i), candidates)).most_common()
        equal = len(counts) == 2 and counts[0][1] == counts[1][1]
        if equal:
            target = on_equal
        elif _type_ == 'most_common':
            target = counts[0][0]
        else:
            target = counts[-1][0]
        return [c for c in candidates if c[i] == target]
    return reducer


@timed
def task_2():
    n = len(lines[0])
    oxygen_generator_rating = (
      reduce(build_reducer('most_common', '1'), range(n), lines)[0]
    )
    co2_scrubber_rating = (
        reduce(build_reducer('least_common', '0'), range(n), lines)[0]
    )
    return str2bin(oxygen_generator_rating) * str2bin(co2_scrubber_rating)


print(f'[Part 1]: {task_1()}')
print(f'[Part 2]: {task_2()}')
