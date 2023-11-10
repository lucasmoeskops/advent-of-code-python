"""
AoC Day 19 - Tractor Beam.
"""

__author__ = "Lucas Moeskops"
__date__ = "2023-11-09"
__summary__ = "Prepare the tractor beam to be able to carry Santa's ship"

from lib import *
from sys import stdin
from y2019.computer import make_runner


def find(x, y):
    out = deque()
    runner = make_runner(program, deque([x, y]), out)
    next(runner)
    return out.pop()


def search_square_fit(x, y, size=100):
    top_col = x
    top_row = y

    while y - top_row < size or x - top_col < size:
        if y - top_row < size:
            top_col += 1
            x = max(top_col, x)

        if x - top_col < size:
            top_row += 1
            y = max(top_row, y)

        while find(top_col, y):
            y += 1
        while find(x, top_row):
            x += 1

    return top_col, top_row


def run():
    in_, out, target = deque(), deque(), (inf, inf)
    grid = grid2d('', default='_')
    for x, y in range2d(0, 0, 50, 50):
        runner = make_runner(program, in_, out)
        in_.extend((x, y))
        next(runner)
        grid[x, y] = out.pop()
    print(sum(1 for x in grid.values() if x == 1))
    x, y = search_square_fit(*max(t for t, v in grid.items() if v == 1))
    print(x * 10000 + y)
    # alignment_sum = 0
    #
    # for x, y in grid:
    #     if grid[x, y] == '#' and all(n in grid and grid[n] == '#' for n in neighbors2d4(x, y)):
    #         alignment_sum += x * y
    #
    # x, y = next(t for t, c in grid.items() if c in '^v<>')
    # route = []
    # last_direction = '^>v<'.index(grid[x, y])
    #
    # while True:
    #     directions = [d for d, n in enumerate(neighbors2d4(x, y)) if d % 2 != last_direction % 2 and grid[n] == '#']
    #     if not directions:
    #         break
    #     assert len(directions) == 1, f"multiple options at {x},{y}"
    #     direction = directions[0]
    #     length = 0
    #     while grid[move2d(x, y, direction)] == '#':
    #         length += 1
    #         x, y = move2d(x, y, direction)
    #     route.append('R' if direction == (last_direction + 1) % 4 else 'L')
    #     route.append(str(length))
    #     last_direction = direction
    #
    # return alignment_sum, ''.join(route)


puzzle = stdin.read()
program = [int(code) for code in puzzle.split(',')]
run()

