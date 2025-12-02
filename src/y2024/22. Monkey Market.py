#!/usr/bin/env python3

"""
AoC Day 22 - Monkey Market - in Python.
"""

__author__ = "Lucas Moeskops"
__date__ = "2024-12-22"
__summary__ = "Back to year 2022, day 11"

from lib import *


puzzle = read_input(2024, 22)
buyers = list(map(int, puzzle.split('\n')))
total = 0
all_values = [0] * 160000
seen = [-1] * 160000

for n, buyer in enumerate(buyers):
    key = buyer % 10 + 10

    for i in range(2000):
        digit = buyer % 10
        # evolve
        buyer = (buyer ^ ((buyer % 0x40000) << 6))
        buyer = (buyer ^ (buyer >> 5)) & 0xFFFFFF
        buyer = (buyer ^ (buyer << 11)) & 0xFFFFFF
        key = (20 * key + buyer % 10 - digit + 10) % 160000
        if seen[key] < n:
            seen[key] = n
            all_values[key] += buyer % 10

    total += buyer

print(total)
print(max(all_values))
