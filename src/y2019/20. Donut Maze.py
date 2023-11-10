"""
AoC Day 20 - Donut Maze
"""

__author__ = "Lucas Moeskops"
__date__ = "2023-11-09"
__summary__ = "The plutonians have made recursive mazes"

from lib import *
from sys import stdin


def get_options(x, y):
    for d, (nx, ny) in enumerate(neighbors2d4(x, y)):
        c = grid[nx, ny]

        if c == '.':
            yield nx, ny

    if (x, y) in portals:
        yield portals[x, y]


def evaluate(x, y):
    return (x, y) == end


puzzle = stdin.read()
grid = grid2d(puzzle)
portals = defaultdict(list)
for t, c in grid.items():
    if c == '.':
        for d, n in enumerate(neighbors2d4(*t)):
            if 'A' <= grid[n] <= 'Z':
                label = grid[n] + grid[move2d(*n, d)]

                if d in (0, 3):
                    label = label[::-1]

                portals[label].append(t)


start = portals['AA'][0]
end = portals['ZZ'][0]
del portals['AA']
del portals['ZZ']
for a, b in list(portals.values()):
    portals[a] = b
    portals[b] = a
routes = bfs([start], curry(get_options), curry(evaluate))
print(len(routes[0]))

min_x, min_y, max_x, max_y = borders2d([k for k, v in grid.items() if v == '.'])
max_x -= 1
max_y -= 1


def on_outside(x, y):
    return x == min_x or y == min_y or x == max_x or y == max_y


def get_options2(x, y, level):
    for d, (nx, ny) in enumerate(neighbors2d4(x, y)):
        c = grid[nx, ny]

        if c == '.':
            yield nx, ny, level

    if (x, y) in portals:
        new_level = level - 1 if on_outside(x, y) else level + 1
        if level >= 0:
            yield *portals[x, y], new_level


def evaluate2(x, y, level):
    return (x, y) == end and level == 0


routes = bfs([(*start, 0)], curry(get_options2), curry(evaluate2))
print(len(routes[0]))
