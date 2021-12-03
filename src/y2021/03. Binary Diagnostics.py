#!/usr/bin/env python3

"""
AoC Day 3 - Binary Diagnostic - in Python.
"""

__author__ = "Lucas Moeskops"
__date__ = "2021-12-03"

from functools import reduce

from sys import stdin

from helpers import timed

full = stdin.read()
lines = full.split('\n')


def str2bin(s):
    return int(''.join(s), 2)


@timed
def task_1():
    gamma_rate = ''.join(max('01', key=p.count) for p in zip(*lines))
    epsilon_rate = gamma_rate.replace('0', '2').replace('1', '0').replace('2', '1')
    return str2bin(gamma_rate) * str2bin(epsilon_rate)


def build_reducer(_filter_, order):
    def reducer(candidates, i):
        col = ''.join(c[i] for c in candidates)
        target = _filter_(order, key=col.count)
        return [c for c in candidates if c[i] == target] or candidates
    return reducer


@timed
def task_2():
    n = len(lines[0])
    oxygen_generator_rating = reduce(build_reducer(max, '10'), range(n), lines)[0]
    co2_scrubber_rating = reduce(build_reducer(min, '01'), range(n), lines)[0]
    return str2bin(oxygen_generator_rating) * str2bin(co2_scrubber_rating)


print(f'[Part 1]: {task_1()}')
print(f'[Part 2]: {task_2()}')
