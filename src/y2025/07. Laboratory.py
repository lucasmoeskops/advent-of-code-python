#!/usr/bin/env python3

"""
AoC Day 7 - Laboratory - in Python.
"""

__author__ = "Lucas Moeskops"
__date__ = "2025-12-07"
__summary__ = "Laboratory"

from collections import Counter

from lib import read_input

puzzle = read_input(2025, 7)
lines = puzzle.split('\n')
# We use a counter to keep track of how many timelines are in each lane
lanes = Counter()
# Start with one timeline at the starting position 'S'
lanes[lines[0].index('S')] = 1
splits = 0

# Look for tachyons and split the timelines that arrive there
for line in lines:
    for i, c in enumerate(line):
        if c == '^' and i in lanes:
            timelines = lanes.pop(i)
            splits += 1
            lanes[i-1] += timelines
            lanes[i+1] += timelines

print(splits)
print(sum(lanes.values()))