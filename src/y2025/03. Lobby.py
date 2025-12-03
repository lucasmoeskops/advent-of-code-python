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

    # Check for both part 1 and part 2
    for i, num_batteries in enumerate((2, 12)):
        pointer = chosen = 0

        # Find best remaining battery choices
        for j in range(num_batteries - 1, -1, -1):
            # Choose best value while leaving enough remaining for the rest
            value = max(line[pointer:length-j])
            # Shorten the search space for remaining batteries
            pointer = line.index(value, pointer) + 1
            chosen = 10 * chosen + ord(value) - ord('0')

        # Update answer with the best choice found
        best_choices[i] += chosen

print(f'{best_choices[0]}\n{best_choices[1]}')  # Part 1 & 2