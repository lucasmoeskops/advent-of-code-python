#!/usr/bin/env python3

"""
AoC Day 2 - Dive! - in Python.
"""

__author__ = "Lucas Moeskops"
__date__ = "2021-12-02"

from operator import itemgetter
from sys import stdin

from helpers import timed, parse_from_re

lines = stdin.read().split('\n')

parser_re = r'(?P<direction>\w+) (?P<amount>\d+)'
parser_map = {'amount': int}
parsed = list(parse_from_re(parser_re, parser_map, lines))


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
