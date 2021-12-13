#!/usr/bin/env python3

"""
AoC Day 13 - Transparent Origami - in Python.
"""

__author__ = "Lucas Moeskops"
__date__ = "2021-12-13"

from functools import reduce
from operator import itemgetter

from sys import stdin

from helpers import timed, parse_from_re

full = stdin.read()
part1, part2 = full.split('\n\n')
dots = [tuple(int(v) for v in dot.split(',')) for dot in part1.split('\n')]
parser_re = r'fold along (?P<axis>\w+)=(?P<point>\d+)'
parser_map = {'point': int}
instructions = list(parse_from_re(parser_re, parser_map, part2.split('\n')))


def make_map():
    return {p for p in dots}


def fold(axis, point, _map_):
    new = set()
    t = 0 if axis == 'x' else 1
    for p in _map_:
        if p[t] < point:
            new.add(p)
        else:
            new.add((-p[t] + 2 * point, p[(t + 1) % 2])[::-1 if t else 1])
    return new


@timed
def task_1():
    getter, _map_ = itemgetter('axis', 'point'), make_map()
    return len(fold(instructions[0]['axis'], instructions[0]['point'], _map_))


def read_map(_map_):
    width, height = (max(column) + 1 for column in zip(*map(itemgetter(0, 1), _map_)))
    for x in range(0, width, 5):  # A C E F H K L Z
        if (x + 3, 5) in _map_:  # A E H K L Z
            if (x + 1, 0) in _map_:  # A E Z
                if (x + 3, 0) in _map_:  # E Z
                    yield 'E' if (x, 1) in _map_ else 'Z'
                else:  # A
                    yield 'A'
            else:  # H K L
                if (x + 3, 0) in _map_:  # H, K
                    yield 'H' if (x + 2, 2) in _map_ else 'K'
                else:  # L
                    yield 'L'
        else:  # C F J
            if (x + 3, 0) in _map_:  # F J
                yield 'F' if (x, 5) in _map_ else 'J'
            else:  # C
                yield 'C'


@timed
def task_2():
    getter = itemgetter('axis', 'point')
    _map_ = reduce(lambda m, i: fold(*i, m), map(getter, instructions), make_map())
    return ''.join(read_map(_map_))


print(f'[Part 1]: {task_1()}')
print(f'[Part 2]: {task_2()}')