"""
AoC Day 5 - Sunny with a Chance of Asteroids - in Python.
"""

__author__ = "Lucas Moeskops"
__date__ = "2020-12-27"

from collections import deque
from functools import reduce
from sys import stdin

def make_runner(program, in_, out_):
    position, program, halted, stalled = 0, program[:], False, False
    while not halted:
        state = position, program, halted, stalled, in_, out_
        position, program, halted, stalled, in_, out_ = step(state)
        if stalled:
            yield program
    yield program

def decode(code):
    yield code % 100
    code //= 100
    for i in range(3):
        yield code % 10
        code //= 10

def eval(program, mode, value):
    return value if mode else program[value]

def step(state):
    position, program, halted, stalled, in_, out_ = state
    operation, *modes = decode(program[position])
    if operation == 1:
        _, a, b, c, *_ = program[position:]
        ma, mb, _ = modes
        program[c] = eval(program, ma, a) + eval(program, mb, b)
        position += 4
    elif operation == 2:
        _, a, b, c, *_ = program[position:]
        ma, mb, _ = modes
        program[c] = eval(program, ma, a) * eval(program, mb, b)
        position += 4
    elif operation == 3 and in_:
        _, a, *_ = program[position:]
        program[a] = in_.popleft()
        position += 2
    elif operation == 4:
        _, a, *_ = program[position:]
        ma, _, _ = modes
        out_.append(eval(program, ma, a))
        position += 2
    elif operation == 5:
        _, a, b, *_ = program[position:]
        ma, mb, _ = modes
        if eval(program, ma, a):
            position = eval(program, mb, b)
        else:
            position += 3
    elif operation == 6:
        _, a, b, *_ = program[position:]
        ma, mb, _ = modes
        if not eval(program, ma, a):
            position = eval(program, mb, b)
        else:
            position += 3
    elif operation == 7:
        _, a, b, c, *_ = program[position:]
        ma, mb, _ = modes
        program[c] = eval(program, ma, a) < eval(program, mb, b)
        position += 4
    elif operation == 8:
        _, a, b, c, *_ = program[position:]
        ma, mb, _ = modes
        program[c] = eval(program, ma, a) == eval(program, mb, b)
        position += 4
    elif operation == 99:
        position += 1
        halted = True
    else:
        stalled = True
    return position, program, halted, stalled, in_, out_

program = [int(line) for line in stdin.read().split(',')]

in_ = deque([1])
out_ = deque()
runner = make_runner(program, in_, out_)
next(runner)
while out_ and not out_[0]:
    out_.popleft()
print(f'1: {out_.popleft()}')

in_ = deque([5])
out_ = deque()
runner = make_runner(program, in_, out_)
next(runner)
print(f'2: {out_.popleft()}')