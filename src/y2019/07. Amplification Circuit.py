"""
AoC Day 7 - Amplification Circuit - in Python.
"""

__author__ = "Lucas Moeskops"
__date__ = "2020-12-28"

from collections import deque
from functools import partial
from itertools import permutations
from sys import stdin

def make_runner(program, in_, out_):
    position, program, halted, stalled = 0, program[:], False, False
    while not halted:
        state = position, program, halted, stalled, in_, out_
        position, program, halted, stalled, in_, out_ = step(state)
        if stalled:
            yield halted, stalled, program
    yield halted, stalled, program

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

print(f'1: {max(map(partial(amplify, program), permutations(range(5), 5)))}')
print(f'2: {max(map(partial(amplify, program), permutations(range(5, 10), 5)))}')