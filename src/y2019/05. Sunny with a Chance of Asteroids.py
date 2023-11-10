"""
AoC Day 5 - Sunny with a Chance of Asteroids - in Python.
"""

__author__ = "Lucas Moeskops"
__date__ = "2020-12-27"
__summary__ = "Upgrade ship for air conditioning to survive Mercurius"

from y2019.computer import make_runner
from lib import *
from sys import stdin


program = [int(line) for line in stdin.read().split(',')]

in_ = deque([1])
out_ = deque()
runner = make_runner(program, in_, out_)
next(runner)

while out_ and not out_[0]:
    out_.popleft()

print(out_.popleft())

in_ = deque([5])
out_ = deque()
runner = make_runner(program, in_, out_)
next(runner)
print(out_.popleft())
