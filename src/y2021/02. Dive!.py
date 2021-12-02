#!/usr/bin/env python3

"""
AoC Day 2 - Dive! - in Python.
"""

__author__ = "Lucas Moeskops"
__date__ = "2021-12-02"

import re
from operator import itemgetter
from sys import stdin

from helpers import timed

full = stdin.read()
lines = full.split('\n')

parser_re = r'(?P<direction>\w+) (?P<amount>\d+)'
parser_map = {'amount': int}
parsed = []
for line in lines:
    if match := re.match(parser_re, line):
        r = match.groupdict()
        for k, v in r.items():
            r[k] = parser_map.get(k, str)(v)
        parsed.append(r)


@timed
def task_1():
    position, depth = 0, 0
    commands = map(itemgetter('direction', 'amount'), parsed)
    for direction, amount in commands:
        match direction:
            case 'forward':
                position += amount
            case 'up':
                depth -= amount
            case 'down':
                depth += amount
    return position * depth


@timed
def task_2():
    aim, position, depth = 0, 0, 0
    commands = map(itemgetter('direction', 'amount'), parsed)
    for direction, amount in commands:
        match direction:
            case 'forward':
                position += amount
                depth += amount * aim
            case 'up':
                aim -= amount
            case 'down':
                aim += amount
    return position * depth


print(f'1: {task_1()}')
print(f'2: {task_2()}')
