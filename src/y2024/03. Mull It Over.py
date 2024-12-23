#!/usr/bin/env python3

"""
AoC Day 3 - Mull It Over - in Python.
"""

__author__ = "Lucas Moeskops"
__date__ = "2024-12-03"
__summary__ = "Back to year 2020, day 2"

from lib import re, read_input

pattern = r'mul\((\d+),(\d+)\)'

puzzle = read_input(2024, 3)
matches = re.findall(pattern, puzzle)
print(sum(int(a) * int(b) for a, b in matches))

total = 0
for part in puzzle.split('do()'):
    enabled, *disabled = part.split('don\'t()')
    matches = re.findall(pattern, enabled)
    total += sum(int(a) * int(b) for a, b in matches)
print(total)