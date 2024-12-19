#!/usr/bin/env python3

"""
AoC Day 5 - Print Queue - in Python.
"""

__author__ = "Lucas Moeskops"
__date__ = "2024-12-05"
__summary__ = "Back to year 2017, day 1"

from lib import defaultdict, read_input


puzzle = read_input(2024, 5)
rules, updates = puzzle.split('\n\n')
graph = defaultdict(set)
graph2 = defaultdict(set)

for rule in rules.split('\n'):
    before, after = map(int, rule.split('|'))
    graph[after].add(before)

good = 0
fixed = 0

for update in updates.split('\n'):
    sequence = list(map(int, update.split(',')))
    middle_index = len(sequence) // 2
    sequence_set = set(sequence)
    correct = True

    for i in range(len(sequence)):
        items_before = len(graph[sequence[i]] & sequence_set)

        if items_before > i:
            correct = False

        if items_before == middle_index:
            value = sequence[i]
            break

    if correct:
        good += value
    else:
        fixed += value

print(good)
print(fixed)
