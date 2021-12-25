#!/usr/bin/env python3

"""
AoC Day 23 - Amphipod - in Python.
"""

__author__ = "Lucas Moeskops"
__date__ = "2021-12-23"

from collections import defaultdict
from heapq import heapify, heappop, heappush
from string import ascii_uppercase
from sys import stdin

from helpers import timed
from helpers import neighbours


def parse_amphipods(lines):
    amphipods = {}
    coords = {(x, y): parse(x, y, v, amphipods) for y, line in enumerate(lines) for x, v in enumerate(line)}
    return amphipods, coords, len(lines) - 3


def parse(x, y, value, amphipods):
    if value in ascii_uppercase:
        amphipods[(x, y)] = value
        return '.'
    return value


lines = stdin.read().strip().split('\n')


def calculate_options(sx, sy, _amphipods_, coords, num):
    options = {}
    self = _amphipods_[(sx, sy)]
    c = "ABCD".index(self)
    start_in_hall = sy == 1
    destination = 3 + 2 * c
    cost = 10**c
    free_destination = all(_amphipods_.get((destination, y), self) == self for y in range(2, 2+num))
    if start_in_hall and not free_destination:
        return options
    if not start_in_hall:
        if destination == sx and free_destination:
            return options
        if any(_amphipods_.get((sx, y)) is not None for y in range(2, sy)):
            return options
        direction = 0
        queue = [(sx - 1, sy * cost), (sx + 1, sy * cost)]
    else:
        direction = 1 if destination > sx else -1
        depth = [y for y in range(1+num, 1, -1) if _amphipods_.get((destination, y)) is None][0]
        queue = [(sx + direction, cost * depth)]
    while queue:
        x, e = queue.pop()
        if coords[(x, 1)] != '.' or (x, 1) in _amphipods_:
            continue
        if start_in_hall and destination == x:
            options[(x, depth)] = e
        if not start_in_hall and coords[(x, 2)] == '#':
            options[(x, 1)] = e
        if direction:
            queue.append((x + direction, e + cost))
        elif x > sx:
            queue.append((x + 1, e + cost))
        else:
            queue.append((x - 1, e + cost))
    return options


def rough_rating(_amphipods_):
    rating = 0
    for (x, y), amphipod in _amphipods_.items():
        i = "ABCD".index(amphipod)
        cost = 10**i
        dest = 3 + 2 * i
        if x != dest:
            rating += cost * (abs(dest-x) + y)
    return rating


def wrongly_placed(_amphipods_, num):
    for (x, y) in _amphipods_:
        if y == 1:
            yield x, y
    for l, x in zip("ABCD", range(3, 11, 2)):
        for y in range(2, 2+num):
            if (x, y) in _amphipods_:
                if _amphipods_[(x, y)] != l:
                    yield x, y
                else:
                    if any(_amphipods_.get((x, z)) != l for z in range(y + 1, 2+num)):
                        yield x, y
                break


def find_cheapest_route(_amphipods_, coords, num):
    heap = [(0, (0, _amphipods_))]
    heapify(heap)
    seen = set()
    best = 1000000
    while heap:
        h, (energy, amphipods_raw) = heappop(heap)
        _amphipods_ = dict(amphipods_raw)
        wrong = list(wrongly_placed(_amphipods_, num))
        options = [
            ((sx, sy), (x, y), e)
            for sx, sy in wrong
            for (x, y), e in calculate_options(sx, sy, _amphipods_, coords, num).items()
        ]
        if not options:
            if not wrong and energy < best:
                best = min(best, energy)
                print('Current best:', best)
        for (sx, sy), (x, y), e in options:
            a = dict(amphipods_raw)
            a[(x, y)] = a[(sx, sy)]
            del a[(sx, sy)]
            r = tuple((x, y, l) for (x, l), l in a.items())
            if r in seen:
                continue
            seen.add(r)
            d = 3 + 2 * "ABCD".index(_amphipods_[(sx, sy)])
            if energy + e + rough_rating(a) < best:
                heappush(heap, (h + (abs(x - d) - abs(sx - d)) * e, (energy + e, a.items())))
    return best


@timed
def task_1():
    amphipods, coords, num = parse_amphipods(lines)
    return find_cheapest_route(amphipods, coords, num)


@timed
def task_2():
    newlines = [*lines[:3], "  #D#C#B#A#  ", "  #D#B#A#C#  ", *lines[3:]]
    amphipods, coords, num = parse_amphipods(newlines)
    return find_cheapest_route(amphipods, coords, num)


print(f'[Part 1]: {task_1()}')
print(f'[Part 2]: {task_2()}')
