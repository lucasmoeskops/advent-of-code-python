#!/usr/bin/env python3

"""
AoC Day 9 - Marble Mania - in Python.
"""

__author__ = "Lucas Moeskops"
__date__ = "2021-12-07"

from collections import deque, defaultdict
from itertools import cycle, count, islice
from sys import stdin

from helpers import timed

words = stdin.read().split(' ')
num_players, num_marbles = map(int, [words[0], words[6]])


def game():
    circle = deque([0])
    scores = defaultdict(int)
    for m, p in zip(count(start=1), cycle(range(num_players))):
        if not m % 23:
            scores[p] += m
            circle.rotate(7)
            scores[p] += circle.pop()
            circle.rotate(-1)
        else:
            circle.rotate(-1)
            circle.append(m)
        yield scores


def determine_winner(_num_marbles_):
    return max(next(islice(game(), _num_marbles_, _num_marbles_ + 1)).values())


@timed
def task_1():
    return determine_winner(num_marbles)


@timed
def task_2():
    return determine_winner(num_marbles * 100)


print(f'[Part 1]: {task_1()}')
print(f'[Part 2]: {task_2()}')
