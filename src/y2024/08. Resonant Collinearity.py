#!/usr/bin/env python3

"""
AoC Day 8 - Resonant Collinearity - in Python.
"""

__author__ = "Lucas Moeskops"
__date__ = "2024-12-08"
__summary__ = "Back to year 2016, day 25"

from lib import count, defaultdict, permutations, read_input


puzzle = read_input(2024, 8)
width = puzzle.index('\n')
height = len(puzzle) // width
antinode_locations_initial = set()
antinode_locations_after_update = set()
antenna_types = defaultdict(list)

for y, row in enumerate(puzzle.split('\n')):
    for n, c in enumerate(row):
        if c != '.' and c != '\n':
            antenna_types[c].append((n, y))

for antenna_type, locations in antenna_types.items():
    for (l1x, l1y), (l2x, l2y) in permutations(locations, 2):
        dx, dy = l2x - l1x, l2y - l1y
        for n in count():
            antenna_x, antenna_y = l1x - dx * n, l1y - dy * n
            if 0 <= antenna_x < width and 0 <= antenna_y < height:
                if n == 1:
                    antinode_locations_initial.add((antenna_x, antenna_y))
                antinode_locations_after_update.add((antenna_x, antenna_y))
            else:
                break

print(len(antinode_locations_initial))
print(len(antinode_locations_after_update))