#!/usr/bin/env python3

"""
AoC Day 13 - Transparent Origami - in Python.
"""

__author__ = "Lucas Moeskops"
__date__ = "2021-12-13"

from collections import deque
from operator import itemgetter
from sys import stdin

from helpers import timed, parse_from_re

part1, part2 = stdin.read().split('\n\n')
dots = [tuple(int(v) for v in dot.split(',')) for dot in part1.split('\n')]
parser_re = r'fold along (?P<axis>\w+)=(?P<pivot>\d+)'
instructions = list(parse_from_re(parser_re, {'pivot': int}, part2.split('\n')))
g = None


def fold(axis, pivot, _map_):
    double = 2 * pivot
    return {
        (
            x if axis == 'y' or x < pivot else double - x,
            y if axis == 'x' or y < pivot else double - y,
        )
        for x, y in _map_
    }


def generator():
    _map_ = set(dots)
    for i in map(itemgetter('axis', 'pivot'), instructions):
        _map_ = fold(*i, _map_)
        yield _map_


@timed
def task_1():
    global g
    g = g or generator()
    return len(next(g))


def read_map(_map_):
    width, height = (max(column) + 1 for column in zip(*map(itemgetter(0, 1), _map_)))
    for x in range(0, width, 5):  # A B C E F G H K L P R U Z
        if (x + 3, 5) in _map_:  # A E G H K L R Z
            if (x + 1, 0) in _map_:  # A E G R Z
                if (x + 3, 0) in _map_:  # E Z
                    yield 'E' if (x, 1) in _map_ else 'Z'
                else:  # A G R
                    if (x, 0) in _map_:  # 'R'
                        yield 'R'
                    else:  # A G
                        yield 'A' if (x, 5) in _map_ else 'G'
            else:  # H K L
                if (x + 3, 0) in _map_:  # H, K
                    yield 'H' if (x + 2, 2) in _map_ else 'K'
                else:  # L
                    yield 'L'
        else:  # B C F J P U
            if (x + 1, 0) in _map_:  # B C F P
                if (x + 2, 2) in _map_:  # B F
                    yield 'F' if (x + 3, 0) in _map_ else 'B'
                else:  # C P
                    yield 'C' if (x + 1, 5) in _map_ else 'P'
            else:  # J U
                yield 'U' if (x, 0) in _map_ else 'J'


@timed
def task_2():
    global g
    g = g or generator()
    return ''.join(read_map(deque(g, maxlen=1).pop()))


print(f'[Part 1]: {task_1()}')
print(f'[Part 2]: {task_2()}')