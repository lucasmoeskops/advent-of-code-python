#!/usr/bin/env python3

"""
AoC Day 15 - Chiton - in Python.
"""

__author__ = "Lucas Moeskops"
__date__ = "2021-15-01"

from heapq import heapify, heappop, heappush
from sys import stdin

from helpers import neighbours, timed

lines = stdin.read().strip().split('\n')
coords = {(x, y): int(v) for y, line in enumerate(lines) for x, v in enumerate(line)}
width, height = len(lines[0]), len(lines)


def risk_level_at(x, y):
    inc_x, x = divmod(x, width)
    inc_y, y = divmod(y, height)
    return ((coords[(x, y)] + inc_x + inc_y) - 1) % 9 + 1


def find_best_route(s, t, multiplier=1, max_local_value=9, cost_eval=risk_level_at):
    global width, height
    map_width = width * multiplier
    map_height = height * multiplier
    worst = sum(abs(ss - st) for ss, st in zip(s, t)) * max_local_value
    heap = [(0, (0, s))]
    heapify(heap)
    best_at = {}
    c = 0
    while heap:
        h, (v, (x, y)) = heappop(heap)
        c += 1
        if best_at.get((x, y), worst) <= v:
            continue
        best_at[(x, y)] = v
        if (x, y) == t:
            return v
        for (nx, ny), dh in zip(neighbours(x, y), [1, -1, -1, 1]):
            if not (0 <= nx < map_width and 0 <= ny < map_height):
                continue
            dv = cost_eval(nx, ny)
            if best_at.get((nx, ny), worst) <= (nv := v + dv):
                continue
            heappush(heap, (h + dv + dh, (nv, (nx, ny))))


@timed
def task_1():
    return find_best_route((0, 0), (width - 1, height - 1))


@timed
def task_2():
    return find_best_route((0, 0), (width * 5 - 1, height * 5 - 1), multiplier=5)


print(f'[Part 1]: {task_1()}')
print(f'[Part 2]: {task_2()}')
