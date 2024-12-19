#!/usr/bin/env python3

"""
AoC Day 11 - Plutonian Pebbles - in Python.
"""

__author__ = "Lucas Moeskops"
__date__ = "2024-12-11"
__summary__ = "Back to year 2019, day 20"

from lib import Counter, read_input


puzzle = list(map(int, read_input(2024, 11).split()))
stones = Counter(puzzle)


def round(counter):
    newcounter = Counter()

    for i, v in counter.items():
        s = str(i)

        if i == 0:
            newcounter[1] += v
        elif len(s) % 2 == 0:
            left, right = int(s[:len(s) // 2]), int(s[len(s) // 2:])
            newcounter[left] += v
            newcounter[right] += v
        else:
            newcounter[2024 * i] += v

    return newcounter


for i in range(75):
    if i == 25:
        print(sum(stones.values()))
    stones = round(stones)
print(sum(stones.values()))
