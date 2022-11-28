#!/usr/bin/env python3

"""
AoC Day 1 - Sonar Sweep - in Python.
"""

__author__ = "Lucas Moeskops"
__date__ = "2021-12-01"


from itertools import pairwise 
from sys import stdin


LINES = stdin.read().split('\n')


def part_1():
    return sum(a < b for a, b in pairwise(ints))


def part_2():
    """ Making use of a + b + c < b + c + d === a < d """
    return sum(a < b for a, b in zip(ints, ints[3:]))


ints = [int(line) for line in LINES if line]


print(part_1())
print(part_2())

