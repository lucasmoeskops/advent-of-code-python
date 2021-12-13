#!/usr/bin/env python3

"""
AoC Day 22 - Mode Maze - in Python.
"""

__author__ = "Lucas Moeskops"
__date__ = "2021-12-13"

from functools import cache
from heapq import heapify, heappop, heappush
from itertools import starmap, product
from sys import stdin

from helpers import timed

full = stdin.read()
lines = full.split('\n')
depth = int(lines[0].split(' ')[1])
target_x, target_y = map(int, lines[1].split(' ')[1].split(','))

ROCKY = 0
WET = 1
NARROW = 2

NEITHER = 0
TORCH = 1
CLIMBING_GEAR = 2


@cache
def erosion_level_at(x, y):
    return (geologic_index_at(x, y) + depth) % 20183


def geologic_index_at(x, y):
    match (x, y):
        case (x, y) if x == target_x and y == target_y:
            return 0
        case (x, y) if x == 0:
            return y * 48271
        case (x, y) if y == 0:
            return x * 16807
    return erosion_level_at(x - 1, y) * erosion_level_at(x, y - 1)


def condition_at(x, y):
    match erosion_level_at(x, y) % 3:
        case 0:
            return ROCKY
        case 1:
            return WET
        case 2:
            return NARROW


def neighbours(x, y):
    return (x, y - 1), (x + 1, y), (x, y + 1), (x - 1, y)


@timed
def task_1():
    return sum(starmap(condition_at, product(range(target_x + 1), range(target_y + 1))))


def find_best_route(x, y):
    worst = (x + y) * 8
    heap = [(worst, (0, 0, 0, condition_at(x, y), TORCH))]
    heapify(heap)
    best_at = {}
    while heap:
        rank, (time, _x_, _y_, condition, gear) = heappop(heap)
        _best_ = best_at.get((_x_, _y_, gear), worst)
        if _best_ <= time:
            continue
        best_at[(_x_, _y_, gear)] = time
        if _x_ == x and _y_ == y and gear == TORCH:
            return time
        for _gear_ in range(3):
            if condition != _gear_ != gear:
                heappush(heap, (rank + 7, (time + 7, _x_, _y_, condition, _gear_)))
        for __x__, __y__ in neighbours(_x_, _y_):
            if __x__ < 0 or __y__ < 0:
                continue
            _condition_ = condition_at(__x__, __y__)
            if not _condition_ == gear:
                _rank_ = time + 1 + abs(__x__ - x) + abs(__y__ - y)
                heappush(heap, (_rank_, (time + 1, __x__, __y__, _condition_, gear)))


@timed
def task_2():
    return find_best_route(target_x, target_y)


print(f'[Part 1]: {task_1()}')
print(f'[Part 2]: {task_2()}')
