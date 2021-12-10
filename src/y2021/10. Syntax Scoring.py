#!/usr/bin/env python3

"""
AoC Day 10 - Syntax Scoring - in Python.
"""

__author__ = "Lucas Moeskops"
__date__ = "2021-12-10"

from functools import reduce
from sys import stdin

from helpers import timed

lines = stdin.read().split('\n')

CLOSER = {'(': ')', '[': ']', '{': '}', '<': '>'}
SYNTAX_SCORE = {')': 3, ']': 57, '}': 1197, '>': 25137}
REPAIR_SCORE = {')': 1, ']': 2, '}': 3, '>': 4}


def syntax_score(line):
    stack = []
    for char in line:
        if char in CLOSER:
            stack.append(CLOSER[char])
        elif char != stack.pop():
            return SYNTAX_SCORE[char]
    return 0


def repair_score(line):
    stack = []
    for char in line:
        if char in CLOSER:
            stack.append(CLOSER[char])
        else:
            stack.pop()
    return reduce(lambda score, c: 5 * score + REPAIR_SCORE[c], stack[::-1], 0)


@timed
def task_1():
    return sum(syntax_score(line) for line in lines)


@timed
def task_2():
    to_repair = [line for line in lines if not syntax_score(line)]
    return sorted(map(repair_score, to_repair))[len(to_repair) // 2]


print(f'[Part 1]: {task_1()}')
print(f'[Part 2]: {task_2()}')
