#!/usr/bin/env python3

"""
AoC Day 2 - Dive! - in Python.
"""

__author__ = "Lucas Moeskops"
__date__ = "2021-12-02"

from sys import stdin


LINES = stdin.read().split('\n')

commands = [(line.split(' ')[0], int(line.split(' ')[1])) for line in LINES]


def part_1():
    position, depth = 0, 0
    for direction, amount in commands:
        match direction:
            case 'forward':
                position += amount
            case 'up':
                depth -= amount
            case 'down':
                depth += amount
    return position * depth


def part_2():
    aim, position, depth = 0, 0, 0
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


print(part_1())
print(part_2())

