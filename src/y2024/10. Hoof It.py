#!/usr/bin/env python3

"""
AoC Day 10 - Disk Fragmenter - in Python.
"""

__author__ = "Lucas Moeskops"
__date__ = "2024-12-10"
__summary__ = "Back to year 2023, day 15"

from lib import *


puzzle = read_input(2024, 10)
width = puzzle.index('\n')
height = puzzle.count('\n') + 1
ints = map(int, puzzle.replace('\n', ''))
plan = list(map(list, batched(ints, width)))
start_positions = [(x, y) for y, row in enumerate(plan) for x, v in enumerate(row) if v == 0]
unique_trailheads = unique_paths = 0

for start_position in start_positions:
    queue = deque([(start_position, 0)])
    unique, distinct = set(), 0

    while queue:
        (x, y), steps = queue.popleft()

        if plan[y][x] == 9:
            distinct += 1
            unique.add((x, y))
            continue

        for dx, dy in ((0, 1), (1, 0), (0, -1), (-1, 0)):
            nx, ny = x + dx, y + dy
            if 0 <= nx < width and 0 <= ny < height and plan[ny][nx] == steps + 1:
                queue.append(((nx, ny), steps + 1))

    unique_paths += distinct
    unique_trailheads += len(unique)

print(unique_trailheads)
print(unique_paths)
