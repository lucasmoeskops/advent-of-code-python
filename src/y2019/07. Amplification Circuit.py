"""
AoC Day 7 - Amplification Circuit - in Python.
"""

__author__ = "Lucas Moeskops"
__date__ = "2020-12-28"
__summary__ = "Install amplifiers in ship to reach Santa in time"

from lib import *
from sys import stdin
from y2019.computer import make_runner


def amplify(program, combination):
    feedback = 5 in combination
    out_ = out0_ = deque([0])
    halt_count = 0
    runners = []

    for i, v in enumerate(combination):
        in_ = out_
        in_.appendleft(v)
        out_ = deque() if i + 1 < len(combination) else out0_
        runners.append(make_runner(program, in_, out_))

    while halt_count < len(combination):
        for runner in list(runners):
            halted, _, _ = next(runner)
            if halted:
                halt_count += 1
                runners.remove(runner)
        if not feedback:
            break

    return out0_.popleft()


program = [int(line) for line in stdin.read().split(',')]

print(max(map(partial(amplify, program), permutations(range(5), 5))))
print(max(map(partial(amplify, program), permutations(range(5, 10), 5))))
