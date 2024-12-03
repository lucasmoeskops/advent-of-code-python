#!/usr/bin/env python3

"""
AoC Day 2 - Cube Conundrum - in Python.
"""

__author__ = "Lucas Moeskops"
__date__ = "2023-12-02"
__summary__ = "You arrive in Snow Island where the water source is dry"

from lib import *


puzzle = read_input(2023, 2)
possible = 0
power = 0
available = {'green': 13, 'red': 12, 'blue': 14}

for id, line in enumerate(lines(puzzle), start=1):
    normalized = line.split(':')[1].replace(',', '').replace(';', '')
    valid = True
    color_max = Counter()

    for amount, color in batched(normalized.split(), 2):
        amount = int(amount)

        if valid and amount > available[color]:
            valid = False

        color_max[color] = max(color_max[color], amount)

    if valid:
        possible += id

    power += prod(color_max.values())

print(possible)
print(power)
