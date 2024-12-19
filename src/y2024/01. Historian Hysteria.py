#!/usr/bin/env python3

"""
AoC Day 1 - Historian Hysteria - in Python.
"""

__author__ = "Lucas Moeskops"
__date__ = "2024-12-01"
__summary__ = "The Chief Historian is lost"

from lib import Counter, batched, read_input

puzzle = read_input(2024, 1)
numbers = map(int, puzzle.split())
matrix = batched(numbers, 2)
left, right = map(sorted, zip(*matrix))
print(sum(abs(a - b) for a, b in zip(left, right)))

occurrences = Counter(right)
print(sum(num * occurrences[num] for num in left))
