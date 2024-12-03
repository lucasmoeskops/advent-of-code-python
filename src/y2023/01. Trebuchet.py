#!/usr/bin/env python3

"""
AoC Day 1 - Trebuchet!? - in Python.
"""

__author__ = "Lucas Moeskops"
__date__ = "2023-12-01"
__summary__ = "To fix the global snowfall the Elves catapult you to the sky"

from lib import *

puzzle = read_input(2023, 1)
pattern = r'one|two|three|four|five|six|seven|eight|nine|0|1|2|3|4|5|6|7|8|9'
lookup = {word: index % 10 for index, word in enumerate(pattern.split('|'), start=1)}
comp, comp_rev = re.compile(pattern), re.compile(pattern[::-1])


def scan_start_1(line):
    for c in line:
        if '0' <= c <= '9':
            return ord(c) - ord('0')


def scan_start_2(line):
    return lookup[re.search(comp, line).group(0)]


def scan_end_2(line):
    return lookup[re.search(comp_rev, line[::-1]).group(0)[::-1]]


lines = puzzle.split('\n')
print(sum(10 * scan_start_1(line) + scan_start_1(line[::-1]) for line in lines))
print(sum(10 * scan_start_2(line) + scan_end_2(line) for line in lines))
