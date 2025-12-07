#!/usr/bin/env python3

"""
AoC Day 19 - Linen Layout - in Python.
"""

__author__ = "Lucas Moeskops"
__date__ = "2024-12-19"
__summary__ = "Back to year 2023, day 12"

from lib import *


puzzle = read_input(2024, 19)
patterns, towels = puzzle.split('\n\n')
patterns = patterns.split(', ')
towels = towels.split('\n')

# These lookups speed things up a lot
pattern_lookup = defaultdict(list)
pattern_lookup2 = defaultdict(list)
pattern_lookup3 = defaultdict(list)

for pattern in patterns:
    if len(pattern) > 2:
        pattern_lookup3[pattern[0:3]].append(pattern)
    elif len(pattern) > 1:
        pattern_lookup2[pattern[0:2]].append(pattern)
    else:
        pattern_lookup[pattern[0]].append(pattern)


@cache
def find_ways_to_make(s):
    total = 0

    for pattern in chain(pattern_lookup[s[0]], pattern_lookup2[s[0:2]], pattern_lookup3[s[0:3]]):
        if s.startswith(pattern):
            if s == pattern:
                total += 1
            else:
                total += find_ways_to_make(s[len(pattern):])

    return total

num_possible = 0
ways = 0

for towel in towels:
    towel_ways = find_ways_to_make(towel)

    if towel_ways:
        num_possible += 1
        ways += towel_ways

print(num_possible)
print(ways)