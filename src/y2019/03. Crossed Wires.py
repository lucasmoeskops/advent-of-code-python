"""
AoC Day 3 - Crossed Wires - in Python.
"""

__author__ = "Lucas Moeskops"
__date__ = "2020-12-27"

from functools import reduce
from sys import stdin

def manhattan_distance(position):
    x, y = position
    return abs(x) + abs(y)

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
    return manhattan_distance((x2 - x1, y2 - y1)) + 1

def touches_at(line, position, x=False):
    nx1, ny1, nx2, ny2 = normalize(line)
    x, y = position
    if nx1 <= x <= nx2 and ny1 <= y <= ny2:
        x1, y1, x2, y2 = line
        return 1 + manhattan_distance((x1 - x, y1 - y))

def intersection_points(wire_1, wire_2):
    lines_1 = tuple(map(normalize, translate_wire(wire_1)))
    lines_2 = tuple(map(normalize, translate_wire(wire_2)))
    for line_1 in lines_1:
        for line_2 in lines_2:
            if p := intersects_at(line_1, line_2):
                yield p

def steps_to(wire, position):
    steps = 0
    for line in translate_wire(wire):
        if x := touches_at(line, position):
            return steps + x
        steps += line_length(line)

wire_1, wire_2 = [line.split(',') for line in stdin.read().split('\n')]

closest_intersection = min(map(manhattan_distance, intersection_points(wire_1, wire_2)))
print(f'1: {closest_intersection}')

total_steps = lambda p: steps_to(wire_1, p) + steps_to(wire_2, p)
fewest_steps = min(map(total_steps, intersection_points(wire_1, wire_2)))
print(f'2: {fewest_steps}')