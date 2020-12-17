#!/usr/bin/env python3

"""
AoC Day 10 - Adapter Array - in Python.

Combination count challenge. Short solution by "abusing" that all
number differences are either 1 or 3, where only 1 generates multiple
combinations. These are calculated independently and multiplied.
"""

__author__ = "Lucas Moeskops"
__date__ = "2020-12-10"

from collections import Counter
from itertools import groupby
from math import prod
from sys import stdin

count = lambda i: 1 + (i - 1) * i // 2

lines = stdin.read().split('\n')
ns = [0, *(sn := sorted(map(int, lines))), sn[-1] + 3]
diffs = [j - i for i, j in zip(ns[:-1], ns[1:])]

print(f'1: {prod(Counter(diffs).values())}')
print(f'2: {prod(count(len([*g])) for k, g in groupby(diffs) if k == 1)}')
