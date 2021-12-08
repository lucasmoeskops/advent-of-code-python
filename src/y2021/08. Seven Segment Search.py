#!/usr/bin/env python3

"""
AoC Day 8 - Seven Segment Search - in Python.
"""

__author__ = "Lucas Moeskops"
__date__ = "2021-12-08"

from sys import stdin

from helpers import timed

lines = stdin.read().split('\n')
displays = [line.split(' | ', 1) for line in lines]


@timed
def task_1():
    return sum(
        1
        for _, output in displays
        for word in output.split(' ')
        if len(word) in (2, 3, 4, 7)
    )


@timed
def task_2():
    total = 0
    for pattern, output in displays:
        words = [''.join(sorted(word)) for word in pattern.split(' ')]
        ordered = sorted(words, key=len)
        to_digit = {
            ordered[0]: '1',
            ordered[1]: '7',
            ordered[2]: '4',
            ordered[9]: '8',
        }
        cmp_1 = set(ordered[0])
        cmp_4 = set(ordered[2])
        for word in ordered[3:6]:
            if len(cmp_1 - set(word)):
                if len(cmp_4 - set(word)) == 2:
                    to_digit[word] = '2'
                else:
                    to_digit[word] = '5'
            else:
                to_digit[word] = '3'

        for word in ordered[6:9]:
            if len(cmp_4 - set(word)):
                if len(cmp_1 - set(word)):
                    to_digit[word] = '6'
                else:
                    to_digit[word] = '0'
            else:
                to_digit[word] = '9'

        display = [''.join(sorted(word)) for word in output.split(' ')]
        total += int(''.join(map(to_digit.get, display)))
    return total


print(f'[Part 1]: {task_1()}')
print(f'[Part 2]: {task_2()}')
