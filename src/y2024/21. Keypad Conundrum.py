#!/usr/bin/env python3

"""
AoC Day 21 - Race Condition - in Python.
"""

__author__ = "Lucas Moeskops"
__date__ = "2024-12-21"
__summary__ = "Back to year 2019, day 25"

from lib import *


NUMERICAL_LOOKUP = {
    '7': (0, 0),
    '8': (1, 0),
    '9': (2, 0),
    '4': (0, 1),
    '5': (1, 1),
    '6': (2, 1),
    '1': (0, 2),
    '2': (1, 2),
    '3': (2, 2),
    '0': (1, 3),
    'A': (2, 3),
}

DIRECTIONAL_LOOKUP = {
    '^': (1, 0),
    'A': (2, 0),
    '<': (0, 1),
    'v': (1, 1),
    '>': (2, 1),
}

puzzle = read_input(2024, 21)
codes = puzzle.split('\n')


def make_route(sx, sy, ex, ey, horizontal_first=True):
    horizontal = ('>' * (ex - sx) if ex > sx else '<' * (sx - ex))
    vertical = ('v' * (ey - sy) if ey > sy else '^' * (sy - ey))
    return ((horizontal + vertical) if horizontal_first else (vertical + horizontal)) + 'A'


def find_numerical_route(code, p=None):
    if not code:
        yield ''
        return

    x, y = p if p else NUMERICAL_LOOKUP['A']
    c = code[0]
    nx, ny = NUMERICAL_LOOKUP[c]

    option_1 = not (x == 0 and ny == 3) and make_route(x, y, nx, ny, horizontal_first=False)
    if option_1:
        for rest in find_numerical_route(code[1:], (nx, ny)):
            yield option_1 + rest

    option_2 = not (nx == 0 and y == 3) and make_route(x, y, nx, ny, horizontal_first=True)
    if option_2 and option_1 != option_2:
        for rest in find_numerical_route(code[1:], (nx, ny)):
            yield option_2 + rest


@cache
def find_directional_route(path):
    if not path:
        return ''

    x, y = DIRECTIONAL_LOOKUP['A']
    route = ''

    for c in path:
        nx, ny = DIRECTIONAL_LOOKUP[c]

        if y != ny and nx == 0:
            route += make_route(x, y, nx, ny, horizontal_first=False)
        else:
            route += make_route(x, y, nx, ny, horizontal_first=True)

        x, y = nx, ny

    return route


def find_directional_route_all(code, p=None):
    if not code:
        yield ''
        return

    x, y = p if p else DIRECTIONAL_LOOKUP['A']
    c = code[0]
    nx, ny = DIRECTIONAL_LOOKUP[c]

    if option_1 := (x != 0 or ny != 0) and make_route(x, y, nx, ny, horizontal_first=False):
        for rest in find_directional_route_all(code[1:], (nx, ny)):
            yield option_1 + rest

    option_2 = (nx != 0 or y != 0) and make_route(x, y, nx, ny, horizontal_first=True)

    if option_2 and option_1 != option_2:
        for rest in find_directional_route_all(code[1:], (nx, ny)):
            yield option_2 + rest


total = 0
for rooms in (2, 25):
    for code in codes:
        best = inf
        for route in find_numerical_route(code):
            parts = route.split('A')
            counter = Counter()
            for part in parts[:-1]:
                counter[part + 'A'] += 1
            for i in range(rooms):
                new_counter = Counter()
                for part, count in counter.items():
                    part_best = inf
                    best_at = 0
                    for option in find_directional_route_all(part):
                        after = find_directional_route(find_directional_route(option))
                        if len(after) < part_best:
                            part_best = len(after)
                            best_at = option
                    new_parts = best_at.split('A')
                    for new_part in new_parts[:-1]:
                        new_counter[new_part + 'A'] += count
                counter = new_counter

            best = min(best, sum(len(part) * count for part, count in counter.items()))
        total += best * int(code[:-1])
    print(total)