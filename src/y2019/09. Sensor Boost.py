"""
AoC Day 9 - Sensor Boost.
"""

__author__ = "Lucas Moeskops"
__date__ = "2021-01-17"
__summary__ = "Complete your computer to catch distress signal from Ceres"


from y2019.computer import make_runner
from lib import *
from sys import stdin


program = [int(line) for line in stdin.read().split(',')]
out = deque()
next(make_runner(program, deque([1]), out))
print(out.pop())

next(make_runner(program, deque([2]), out))
print(out.pop())
