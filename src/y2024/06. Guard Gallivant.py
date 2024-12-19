#!/usr/bin/env python3

"""
AoC Day 6 - Guard Gallivant - in Python.
"""

__author__ = "Lucas Moeskops"
__date__ = "2024-12-06"
__summary__ = "Back to year 2018, day 5"

from lib import batched, read_input

LOOP = 1
EXIT = 2


def walk(x, y, dx, dy):
    seen = set()

    while True:
        if (x, y, dx, dy) in seen:
            return LOOP

        seen.add((x, y, dx, dy))

        nx, ny = x + dx, y + dy

        if not (0 <= nx < width and 0 <= ny < height):
            return EXIT

        while puzzle[ny][nx] == '#':
            dx, dy = -dy, dx
            nx, ny = x + dx, y + dy

        yield x, y, dx, dy, nx, ny

        x = nx
        y = ny


puzzle = read_input(2024, 6)
width = puzzle.index('\n')
height = puzzle.count('\n')+1
start_y, start_x = divmod(puzzle.index('^'), width+1)
puzzle = list(map(list, batched(puzzle.replace('\n', ''), width)))
steps = 1
obstacle_locations = set()
for x, y, dx, dy, next_x, next_y in walk(start_x, start_y, 0, -1):
    if puzzle[y][x] != 'X':
        steps += 1
        puzzle[y][x] = 'X'

    if puzzle[next_y][next_x] != 'X':
        puzzle[next_y][next_x] = '#'
        try:
            gen = walk(x, y, -dy, dx)
            while True:
                next(gen)
        except StopIteration as e:
            if e.value == LOOP:
                obstacle_locations.add((next_x, next_y))
        puzzle[next_y][next_x] = '.'

print(steps)
print(len(obstacle_locations))