#!/usr/bin/env python3

"""
AoC Day 25 - Code Chronicle - in Python.
"""

__author__ = "Lucas Moeskops"
__date__ = "2024-12-25"
__summary__ = "Back to the Chief's Office "

from lib import *


puzzle = read_input(2024, 25)
keys = puzzle.split('\n\n')
tops, bottoms = [], []
max_height = 5

def fits(left, right):
    return all(l + r <= max_height for l, r in zip(left, right))

for key in keys:
    rows = key.split('\n')
    heights = []
    for col in zip(*rows):
        heights.append(col.count('#') - 1)
    (bottoms if key[0] == '.' else tops).append(heights)

print(sum(1 for top in tops for bottom in bottoms if fits(top, bottom)))  # Part 1
print('')  # Part 2 has no answer