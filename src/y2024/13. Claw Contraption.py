#!/usr/bin/env python3

"""
AoC Day 13 - Claw Contraption - in Python.
"""

__author__ = "Lucas Moeskops"
__date__ = "2024-12-13"
__summary__ = "Back to year 2020, day 24"

from lib import bisect_left, ints, read_input


def test_number(ax, ay, bx, by, px, py):
    def binsearch(b):
        a = (px - b * bx) / ax
        ratio = (a * ax + b * bx) / (a * ay + b * by)
        return ratio

    if ax/ay > bx/by:
        answer = px//bx - bisect_left(range(px//bx, -1, -1), px/py, key=binsearch)
    else:
        answer = bisect_left(range(px//bx+1), px/py, key=binsearch)

    if answer.is_integer():
        a = (px - answer * bx) // ax
        if a * ax + answer * bx == px and a * ay + answer * by == py:
            return a * 3 + answer

    return 0


puzzle = read_input(2024, 13)
blocks_raw = puzzle.split('\n\n')
blocks = []

for block in blocks_raw:
    a, b, prize = block.split('\n')
    blocks.append((ints(a), ints(b), ints(prize)))

for addition in (0, 10000000000000):
    cheapest = 0

    for block in blocks:
        (ax, ay), (bx, by), (px, py) = block
        px += addition
        py += addition
        cheapest += test_number(ax, ay, bx, by, px, py)

    print(cheapest)
