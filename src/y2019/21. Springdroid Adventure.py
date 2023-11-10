"""
AoC Day 21 - Springdroid Adventure.
"""

__author__ = "Lucas Moeskops"
__date__ = "2023-11-09"
__summary__ = "Investigate ship hull damage with a jumping droid"

from lib import *
from sys import stdin
from y2019.computer import make_runner


def walk():
    in_, out, target = deque(), deque(), (inf, inf)
    droidscript = [
        "NOT A J\n",
        "NOT B T\n",
        "NOT T T\n",
        "OR C T\n",
        "NOT T T\n",
        "AND D T\n",
        "OR T J\n",
        "NOT C T\n",
        "AND D T\n",
        "OR T J\n",
        "WALK\n",
    ]
    runner = make_runner(program, in_, out)
    next(runner)
    out.clear()
    in_.extend(chain.from_iterable(map(ord, line) for line in droidscript))
    next(runner)
    print(out.pop())


puzzle = stdin.read()
program = [int(code) for code in puzzle.split(',')]
walk()


def run():
    in_, out, target = deque(), deque(), (inf, inf)
    droidscript = [
        "NOT A J\n",
        "NOT H T\n",
        "NOT T T\n",
        "AND B T\n",
        "AND C T\n",
        "NOT T T\n",
        "AND D T\n",
        "AND H T\n",
        "OR T J\n",
        "RUN\n",
    ]
    runner = make_runner(program, in_, out)
    next(runner)
    out.clear()
    in_.extend(chain.from_iterable(map(ord, line) for line in droidscript))
    next(runner)
    print(out.pop())


run()