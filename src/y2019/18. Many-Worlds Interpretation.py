"""
AoC Day 18 - Many-Worlds Interpretation
"""

__author__ = "Lucas Moeskops"
__date__ = "2023-11-09"
__summary__ = "Collect keys in massive vault where pulled by tractor beam"

from lib import *
from sys import stdin


def get_options(x, y, keys):
    for nx, ny in neighbors2d4(x, y):
        c = grid[nx, ny]

        if c == '.':
            yield nx, ny, keys

        if 'a' <= c <= 'z':
            yield nx, ny, keys | 1 << ord(c) - ord('a')

        if 'A' <= c <= 'Z' and keys & 1 << ord(c) - ord('A'):
            yield nx, ny, keys


def evaluate(x, y, keys):
    return keys == total_key_value


puzzle = stdin.read()
grid = grid2d(puzzle)
total_key_value = 2**26 - 1
start = next(t for t, c in grid.items() if c == '@')
routes = bfs([(*start, 0)], curry(get_options), curry(evaluate))
print(len(routes[0]))

x, y = start
for t in range2d(x-1, y-1, x+2, y+2):
    grid[t] = '.'
grid[start] = '#'
for d in range(4):
    grid[move2d(*start, d)] = '#'


def get_options2(x1, y1, x2, y2, x3, y3, x4, y4, keys):

    for nx, ny in neighbors2d4(x1, y1):
        if (nx, ny, keys) not in seen_extra:
            seen_extra.add((nx, ny, keys))
        else:
            continue
        c = grid[nx, ny]

        if c == '.':
            yield nx, ny, x2, y2, x3, y3, x4, y4, keys

        if 'a' <= c <= 'z':
            yield nx, ny, x2, y2, x3, y3, x4, y4, keys | 1 << ord(c) - ord('a')

        if 'A' <= c <= 'Z' and keys & 1 << ord(c) - ord('A'):
            yield nx, ny, x2, y2, x3, y3, x4, y4, keys

    for nx, ny in neighbors2d4(x2, y2):
        if (nx, ny, keys) not in seen_extra:
            seen_extra.add((nx, ny, keys))
        else:
            continue
        c = grid[nx, ny]

        if c == '.':
            yield x1, y1, nx, ny, x3, y3, x4, y4, keys

        if 'a' <= c <= 'z':
            yield x1, y1, nx, ny, x3, y3, x4, y4, keys | 1 << ord(c) - ord('a')

        if 'A' <= c <= 'Z' and keys & 1 << ord(c) - ord('A'):
            yield x1, y1, nx, ny, x3, y3, x4, y4, keys

    for nx, ny in neighbors2d4(x3, y3):
        if (nx, ny, keys) not in seen_extra:
            seen_extra.add((nx, ny, keys))
        else:
            continue
        c = grid[nx, ny]

        if c == '.':
            yield x1, y1, x2, y2, nx, ny, x4, y4, keys

        if 'a' <= c <= 'z':
            yield x1, y1, x2, y2, nx, ny, x4, y4, keys | 1 << ord(c) - ord('a')

        if 'A' <= c <= 'Z' and keys & 1 << ord(c) - ord('A'):
            yield x1, y1, x2, y2, nx, ny, x4, y4, keys

    for nx, ny in neighbors2d4(x4, y4):
        if (nx, ny, keys) not in seen_extra:
            seen_extra.add((nx, ny, keys))
        else:
            continue
        c = grid[nx, ny]

        if c == '.':
            yield x1, y1, x2, y2, x3, y3, nx, ny, keys

        if 'a' <= c <= 'z':
            yield x1, y1, x2, y2, x3, y3, nx, ny, keys | 1 << ord(c) - ord('a')

        if 'A' <= c <= 'Z' and keys & 1 << ord(c) - ord('A'):
            yield x1, y1, x2, y2, x3, y3, nx, ny, keys


def evaluate2(x1, y1, x2, y2, x3, y3, x4, y4, keys):
    return keys == total_key_value


seen_extra = set()
routes = bfs([(x-1, y-1, x-1, y+1, x+1, y+1, x+1, y-1, 0)], curry(get_options2), curry(evaluate2))
print(len(routes[0]))
