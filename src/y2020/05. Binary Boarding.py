#!/usr/bin/env python3

"""
AoC Day 5 - Binary Boarding - in Python.

The seats can be read as binary number in one operation. Finding the
missing one by comparing the sorted list with an enumerated compare-list.
"""

__author__ = "Lucas Moeskops"
__date__ = "2020-12-05"

from itertools import dropwhile
from sys import stdin

to_int = lambda s, cs: int(''.join('1' if c in cs else '0' for c in s), 2)

find_missing = (lambda sorted_seats:
    next(dropwhile(lambda t: t[0] == t[1], enumerate(sorted_seats, sorted_seats[0])))[0])

lines = stdin.read().split('\n')
sorted_seats = sorted(to_int(line, 'BR') for line in lines)

print(f'1: {sorted_seats[-1]}')
print(f'2: {find_missing(sorted_seats)}')
