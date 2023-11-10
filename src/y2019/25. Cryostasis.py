"""
AoC Day 25 - xxx.
"""

__author__ = "Lucas Moeskops"
__date__ = "2023-11-10"
__summary__ = ""

from lib import *
from sys import stdin
from y2019.computer import make_runner


def computer():
    in_, out = deque([]), deque()
    runner = make_runner(program, in_, out)
    next(runner)
    while True:
        value = yield out
        out.clear()
        in_.extend(value)
        next(runner)


def run():
    cpu = computer()
    out = next(cpu)
    while True:
        print(''.join(map(chr, out)))
        value = ''
        while value not in ['north', 'south', 'east', 'west', 'inv'] and not value.startswith('take ') and not value.startswith('drop '):
            value = input()
        out = cpu.send([*map(ord, value + '\n')])

# We need the monolith, the antenna, the semiconductor and the food ration to pass the security checkpoint
plan = [
    'east',
    'east',
    'take semiconductor',
    'east',
]

# puzzle = stdin.read()
puzzle = open('../../data/y2019/25.txt', 'r').read()
program = [int(code) for code in puzzle.split(',')]
run()
