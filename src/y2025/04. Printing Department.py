#!/usr/bin/env python3

"""
AoC Day 3 - Printing Department - in Python.
"""

__author__ = "Lucas Moeskops"
__date__ = "2025-12-03"
__summary__ = "Printing Department"

from collections import deque

from lib import read_input, neighbors2d8


MAX_ADJACENT_PAPER_ROLLS = 3

puzzle = read_input(2025, 4)

# Read locations into a set
roll_locations = set()
row = offset = 0

for i, c in enumerate(puzzle):
    if c == '\n':
        row += 1
        offset = i + 1
    elif c == '@':
        roll_locations.add((i - offset, row))

# To keep track of answers
initial_locations = len(roll_locations)
can_remove = False
delayed_removals = set()

# Initialize the processing queue
queue = deque(roll_locations)
# For the answer of part 1 we need to know when the first iteration ends
first_iteration = len(roll_locations)

# Simulate until no more rolls can be removed
while queue:
    # Logic for part 1 when the first iteration ends
    if first_iteration:
        first_iteration -= 1

        if not first_iteration:
            can_remove = True
            roll_locations = roll_locations - delayed_removals
            queue.extend(delayed_removals)

    x, y = queue.popleft()

    # Skip if the roll was already removed
    if (x, y) not in roll_locations:
        continue

    # Check if the roll can be removed
    adjacents = []

    for dx, dy in neighbors2d8(x, y):
        if (dx, dy) in roll_locations:
            adjacents.append((dx, dy))

            if len(adjacents) > MAX_ADJACENT_PAPER_ROLLS:
                break
    else:
        # Remove the roll if possible
        roll_locations.remove((x, y)) if can_remove else delayed_removals.add((x, y))

        # Enqueue adjacent rolls for re-evaluation
        queue.extend(adjacents)

print(len(delayed_removals))  # Part 1
print(initial_locations - len(roll_locations))  # Part 2
