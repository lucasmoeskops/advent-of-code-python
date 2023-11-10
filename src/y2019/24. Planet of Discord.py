"""
AoC Day 24 - Planet of Discord
"""

__author__ = "Lucas Moeskops"
__date__ = "2023-11-10"
__summary__ = "Analyse bug reproduction on Eris"

from lib import *
from sys import stdin


def transform(state):
    new_state = grid2d('')
    for p in range2d(0, 0, 5, 5):
        c = state[p]
        bug_neighbours = sum(1 for n in neighbors2d4(*p) if state[n] == '#')
        if c == '#' and bug_neighbours != 1:
            new_state[p] = '.'
        elif c != '#' and 1 <= bug_neighbours <= 2:
            new_state[p] = '#'
        else:
            new_state[p] = state[p]
    return new_state


def recursive_neighbours(x, y, level):
    # up
    if y == 0:
        yield 2, 1, level - 1
    elif x == 2 and y == 3:
        for i in range(5):
            yield i, 4, level + 1
    else:
        yield x, y - 1, level

    # right
    if x == 4:
        yield 3, 2, level - 1
    elif x == 1 and y == 2:
        for i in range(5):
            yield 0, i, level + 1
    else:
        yield x + 1, y, level

    # down
    if y == 4:
        yield 2, 3, level - 1
    elif x == 2 and y == 1:
        for i in range(5):
            yield i, 0, level + 1
    else:
        yield x, y + 1, level

    # left
    if x == 0:
        yield 1, 2, level - 1
    elif x == 3 and y == 2:
        for i in range(5):
            yield 4, i, level + 1
    else:
        yield x - 1, y, level


def recursive_transform(state):
    new_state = defaultdict(lambda: grid2d('', default='.'))
    min_level = min(state)
    max_level = max(state)

    for level in range(min_level-1, max_level+2):
        grid = state[level]
        for p in range2d(0, 0, 5, 5):
            if p == (2, 2):
                continue
            c = grid[p]
            bug_neighbours = sum(1 for x, y, level in recursive_neighbours(*p, level) if state[level][x, y] == '#')
            if c == '#' and bug_neighbours == 1:
                new_state[level][p] = '#'
            elif c != '#' and 1 <= bug_neighbours <= 2:
                new_state[level][p] = '#'
    return new_state


puzzle = stdin.read()
grid = grid2d(puzzle)
start, length = period(transform_stream(transform, grid), key=lambda x: tuple(x.values()))
final_grid = nth(transform_stream(transform, grid), start)
print(sum(2**i for i, c in enumerate(tuple(final_grid.values())) if c == '#'))
result = nth(transform_stream(recursive_transform, defaultdict(lambda: grid2d('', default='.'), {0: grid})), 200)
print(sum(1 for grid in result.values() for cell in grid.values() if cell == '#'))