#!/usr/bin/env python3

"""
AoC Day 12 - Rain Risk - in Python.

2D Grid movement puzzle. Solved with many short functions.
"""

__author__ = "Lucas Moeskops"
__date__ = "2020-12-12"

from functools import reduce
from sys import stdin

manhattan = lambda x, y: abs(x) + abs(y)

move = lambda m, a, p: reduce(lambda q, b: m[b[0]](*q, int(b[1:])), a, p)

north    = lambda x, y, d, n: (    x, y - n,             d)
south    = lambda x, y, d, n: (    x, y + n,             d)
east     = lambda x, y, d, n: (x + n,     y,             d)
west     = lambda x, y, d, n: (x - n,     y,             d)
left     = lambda x, y, d, n: (    x,     y, (d - n) % 360)
right    = lambda x, y, d, n: (    x,     y, (d + n) % 360)
forwards = lambda x, y, d, n: directions[d](x, y, d, n)

north2    = lambda x, y, wx, wy, n: (x, y,     wx, wy - n)
south2    = lambda x, y, wx, wy, n: (x, y,     wx, wy + n)
east2     = lambda x, y, wx, wy, n: (x, y, wx + n,     wy)
west2     = lambda x, y, wx, wy, n: (x, y, wx - n,     wy)
left2     = (lambda x, y, wx, wy, n:
    (x, y, wx, wy) if n == 0 else  left2(x, y,  wy, -wx, n - 90))
right2    = (lambda x, y, wx, wy, n:
    (x, y, wx, wy) if n == 0 else right2(x, y, -wy,  wx, n - 90))
forwards2 = lambda x, y, wx, wy, n: (x + n * wx, y + n * wy, wx, wy)

mapping1 = {
    'N': north, 'S': south, 'E': east, 'W': west, 'L': left, 'R': right, 'F': forwards}
mapping2 = {
    'N': north2, 'S': south2, 'E':     east2, 'W': west2,
    'L':  left2, 'R': right2, 'F': forwards2}

directions = {0: east, 90: south, 180: west, 270: north}

lines = stdin.read().split('\n')

x, y, d = move(mapping1, lines, (0, 0, 0))
print(f'1: {manhattan(x, y)}')

x, y, wx, wy = move(mapping2, lines, (0, 0, 10, -1))
print(f'2: {manhattan(x, y)}')