#!/usr/bin/env python3

"""
AoC Day 3 - Lobby - in Python.
"""

__author__ = "Lucas Moeskops"
__date__ = "2025-12-03"
__summary__ = "Lobby"

from itertools import count

from lib import read_input

puzzle = read_input(2025, 3)

lines = puzzle.split('\n')
# Stores answers for part 1 and 2
best_choices = [0, 0]

for line in lines:
    length = len(line)
    # Convert line to list of integers
    values = [*map(int, line)]

    # Check for both part 1 and part 2
    for i, num_batteries in enumerate((2, 12)):
        pointer = 0
        chosen = []

        # Find best remaining battery choices
        for j in range(num_batteries - 1, -1, -1):
            # Choose best value while leaving enough remaining for the rest
            value = max(values[pointer:length-j])
            # Shorten the search space for remaining batteries
            pointer = values.index(value, pointer) + 1
            chosen.append(value)

        # Update answer with best choice found
        best_choices[i] += int(''.join(map(str, chosen)))

print(f'{best_choices[0]}\n{best_choices[1]}')  # Part 1 & 2