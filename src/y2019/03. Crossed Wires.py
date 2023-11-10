"""
AoC Day 3 - Crossed Wires - in Python.
"""

__author__ = "Lucas Moeskops"
__date__ = "2020-12-27"
__summary__ = "Complete fuel management system for tanking on Venus"

from lib import *
from sys import stdin


def translate(line, position):
    direction, *length = line
    length = int(''.join(length))
    x, y = position
    if direction == 'U':
        return x, y - 1, x, y - length
    if direction == 'R':
        return x + 1, y, x + length, y
    if direction == 'D':
        return x, y + 1, x, y + length
    if direction == 'L':
        return x - 1, y, x - length, y


def normalize(line):
    x1, y1, x2, y2 = line
    return min(x1, x2), min(y1, y2), max(x1, x2), max(y1, y2)


def intersects_at(line_1, line_2):
    l1x1, l1y1, l1x2, l1y2 = line_1
    l2x1, l2y1, l2x2, l2y2 = line_2
    ipx, ipy = (l1x1, l2y1) if l1x1 == l1x2 else (l2x1, l1y1)
    if l1x1 <= ipx <= l1x2 and l2x1 <= ipx <= l2x2\
            and l1y1 <= ipy <= l1y2 and l2y1 <= ipy <= l2y2:
        return ipx, ipy


def translate_wire(wire):
    p = (0, 0)
    for cmd in wire:
        line = translate(cmd, p)
        yield line
        p = line[-2:]


def line_length(line):
    x1, y1, x2, y2 = line
    return manhattan2d(0, 0, x2 - x1, y2 - y1) + 1


def touches_at(line, x, y):
    nx1, ny1, nx2, ny2 = normalize(line)

    if nx1 <= x <= nx2 and ny1 <= y <= ny2:
        x1, y1, x2, y2 = line
        return 1 + manhattan2d(0, 0, x1 - x, y1 - y)


def intersection_points(wire_1, wire_2):
    lines_1 = tuple(map(normalize, translate_wire(wire_1)))
    lines_2 = tuple(map(normalize, translate_wire(wire_2)))
    for line_1 in lines_1:
        for line_2 in lines_2:
            if p := intersects_at(line_1, line_2):
                yield p


def steps_to(wire, x, y):
    steps = 0
    for line in translate_wire(wire):
        if x_ := touches_at(line, x, y):
            return steps + x_
        steps += line_length(line)


def total_steps(x, y):
    return steps_to(wire_1, x, y) + steps_to(wire_2, x, y)


wire_1, wire_2 = [line.split(',') for line in stdin.read().split('\n')]

closest_intersection = min(starmap(partial(manhattan2d, 0, 0), intersection_points(wire_1, wire_2)))
print(closest_intersection)

fewest_steps = min(map(curry(total_steps), intersection_points(wire_1, wire_2)))
print(fewest_steps)
