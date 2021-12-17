#!/usr/bin/env python3

"""
AoC Day 17 - Trick Shot - in Python.
"""

__author__ = "Lucas Moeskops"
__date__ = "2021-12-17"

from operator import itemgetter
from sys import stdin

from math import sqrt, ceil

from helpers import timed, parse_from_re

full = stdin.read().strip()
input_re = (
    r'target area: x=(?P<sx>[\w-]+)..(?P<ex>[\w-]+), y=(?P<sy>[\w-]+)..(?P<ey>[\w-]+)'
)
mapper = {'sx': int, 'ex': int, 'sy': int, 'ey': int}
sx, ex, sy, ey = itemgetter('sx', 'ex', 'sy', 'ey')(
    next(parse_from_re(input_re, mapper, [full]))
)
max_height = -1
distinct_solutions = set()


def calc_trajectory(f, v):
    x, y, h = 0, 0, 0
    while (
        abs(x) <= max(abs(ex), abs(sx)) and y >= min(sy, ey)  # in bounds
        and not (sx <= x <= ex and sy <= y <= ey)  # in target area
    ):
        x += f
        f = f - 1 if f > 0 else (f + 1 if f < 0 else f)
        y += v
        v -= 1
        h = max(h, y)
    return h if sx <= x <= ex and sy <= y <= ey else -1


def calculate():
    global max_height, distinct_solutions
    min_f = ceil((-1 + sqrt(1 + 8 * sx)) / 2)  # abc-formula
    for f in range(min_f, max(abs(sx), abs(ex)) + 1):
        # after f steps in target area
        # min_v = 11  #
        for v in range(-max(abs(ey), abs(sy)), abs(ey - sy) * 4):
            solution_max_height = calc_trajectory(f, v)
            if solution_max_height >= 0:
                distinct_solutions.add((f, v))
            max_height = max(max_height, solution_max_height)


@timed
def task_1():
    if max_height < 0:
        calculate()
    return max_height


@timed
def task_2():
    if len(distinct_solutions) == 0:
        calculate()
    return len(distinct_solutions)


print(f'[Part 1]: {task_1()}')
print(f'[Part 2]: {task_2()}')
