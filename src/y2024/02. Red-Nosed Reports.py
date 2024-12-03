#!/usr/bin/env python3

"""
AoC Day 2 - Red-Nosed Reports - in Python.
"""

__author__ = "Lucas Moeskops"
__date__ = "2024-12-02"
__summary__ = "The Chief Historian is not in the nuclear fusion plant"

from lib import *


def find_issue(report):
    decreasing = (report[0] > report[1]) + (report[1] > report[2]) + (report[2] > report[3]) > 1

    for i, (a, b) in enumerate(pairwise(report)):
        if (b > a) == decreasing or not (1 <= abs(b - a) <= 3):
            return i

    return -1

puzzle = read_input(2024, 2)
safe = 0
safe_with_dampener = 0

for line in puzzle.split('\n'):
    report = list(map(int, line.split()))
    issue = find_issue(report)

    if issue == -1:
        safe += 1
    elif find_issue(report[:issue] + report[issue+1:]) == -1:
        safe_with_dampener += 1
    elif find_issue(report[:issue+1] + report[issue+2:]) == -1:
        safe_with_dampener += 1

print(safe)
print(safe + safe_with_dampener)
