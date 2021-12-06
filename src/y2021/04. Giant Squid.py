#!/usr/bin/env python3

"""
AoC Day 4 - Giant Squid - in Python.
"""

__author__ = "Lucas Moeskops"
__date__ = "2021-12-05"

from collections import Counter
from dataclasses import dataclass
from itertools import islice, chain, cycle, repeat, count

from sys import stdin

from helpers import timed


class Board:
    def __init__(self, data):
        self.lookup = {value: i for i, value in enumerate(data)}
        self.row_status = Counter()
        self.col_status = Counter()
        self.final_value = None

    def play(self, value):
        self.final_value = value
        i = self.lookup.get(value)
        if i is not None:
            y, x = divmod(i, 5)
            del self.lookup[value]
            row_value = self.row_status[x] = self.row_status[x] + 1
            if row_value == 5:
                return True
            col_value = self.col_status[y] = self.col_status[y] + 1
            if col_value == 5:
                return True
        return False

    def score(self):
        return self.final_value * sum(self.lookup.keys())


def cut(length, iterator):
    piece = list(islice(iterator, length))
    if not piece:
        return
    yield piece
    yield from cut(length, iterator)


lines = stdin.read().split('\n')
picking_order = [int(v) for v in lines[0].split(',')]
boards_data = cut(6, iter(lines[2:]))
boards_data = [
    [int(v) for v in ' '.join(line for line in data).split()]
    for data in boards_data
]


@timed
def task_1():
    boards = [Board(board_data) for board_data in boards_data]

    for value in picking_order:
        for board in boards:
            if board.play(value):
                return board.score()


@timed
def task_2():
    boards = {n: Board(data) for n, data in enumerate(boards_data)}

    for value in picking_order:
        for n, board in list(boards.items()):
            if board.play(value):
                del boards[n]
                if len(boards) == 0:
                    return board.score()


print(f'[Part 1]: {task_1()}')
print(f'[Part 2]: {task_2()}')
