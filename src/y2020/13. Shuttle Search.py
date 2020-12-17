#!/usr/bin/env python3

"""
AoC Day 13 - Shuttle Search - in Python.

Chinese remainder theorem like exercise. Solved with a bit
different approach, that just efficiently searches matches.
"""

__author__ = "Lucas Moeskops"
__date__ = "2020-12-13"

from functools import reduce
from itertools import count
from operator import itemgetter
from sys import stdin

soonest_bus = (lambda timestamp, buses:
    min(((t, timestamp // t * t + t - timestamp) for t, _ in buses), key=itemgetter(1)))

aligned = (lambda buses:
    count(*reduce((lambda cur, bus:
        (lambda t, d, i, p: (next(j - d for j in count(i + d, p) if not j % t), p * t))
        (*bus, *cur)), buses, (1, 1))))

timestamp, times = stdin.read().split('\n')
timestamp = int(timestamp)
buses = [(int(t), d) for d, t in enumerate(times.split(',')) if t != 'x']

bus, minutes_late = soonest_bus(timestamp, buses)

print(f'1: {bus * minutes_late}')
print(f'2: {next(aligned(buses))}')