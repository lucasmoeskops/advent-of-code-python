#!/usr/bin/env python3

"""
AoC Day 5 - Cafetaria - in Python.
"""

__author__ = "Lucas Moeskops"
__date__ = "2025-12-05"
__summary__ = "Cafetaria"

from collections import deque

from lib import read_input, neighbors2d8

puzzle = read_input(2025, 5)

# Parse input
fresh_ranges, ingredient_ids = puzzle.split('\n\n')
fresh_ranges = [tuple(map(int, line.split('-'))) for line in fresh_ranges.split('\n')]
fresh_ranges.sort()
ingredient_ids = [*map(int, ingredient_ids.split('\n'))]
ingredient_ids.sort()

# Part 1
num_fresh = 0
range_pointer = 0

for ingredient_id in ingredient_ids:
    while range_pointer < len(fresh_ranges) and ingredient_id > fresh_ranges[range_pointer][1]:
        range_pointer += 1

    if range_pointer == len(fresh_ranges):
        break

    if ingredient_id >= fresh_ranges[range_pointer][0]:
        num_fresh += 1

# Walk through the ranges to count total fresh ingredients for part 2
total_fresh = 0
end_pointer = 0

for low, high in fresh_ranges:
    start_pointer = max(end_pointer, low)
    if start_pointer <= high:
        total_fresh += (high - start_pointer + 1)
        end_pointer = high + 1

print(num_fresh)  # Part 1
print(total_fresh)  # Part 2