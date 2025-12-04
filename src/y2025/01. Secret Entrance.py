#!/usr/bin/env python3

"""
AoC Day 1 - Secret Entrance - in Python.
"""

__author__ = "Lucas Moeskops"
__date__ = "2025-12-01"
__summary__ = "Secret Entrance"

from lib import read_input

puzzle = read_input(2025, 1)
commands = puzzle.split('\n')
previous = 50  # Starting position
SIZE = 100  # Size of the wheel
times_zero = 0  # Counter for part 1
times_past_zero = 0  # Counter for part 2

for command in commands:
    # Extract the values from the command
    direction, amount = command[0], command[1:]
    amount = int(amount)

    # Calculate the new position from the previous position
    new = previous + amount if direction == 'R' else previous - amount

    # For part 1: if we end up on 0, count it.
    if new % SIZE == 0:
        times_zero += 1

    # Calculate the difference in rotations between previous and current
    times_past_zero += abs(new // SIZE - previous // SIZE)

    # Edge cases occur only when new is smaller than previous
    if new < previous:
        # Edge case: it moved from zero with a LEFT direction
        if previous % SIZE == 0:
            times_past_zero -= 1

        # Edge case: it moved to zero from a LEFT direction
        if new % SIZE == 0:
            times_past_zero += 1

    # Reset previous for the next command
    previous = new

print(times_zero)  # Part 1
print(times_past_zero)  # Part 2