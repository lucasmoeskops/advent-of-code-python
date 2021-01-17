"""
AoC Day 9 - Sensor Boost.
"""

__author__ = "Lucas Moeskops"
__date__ = "2021-01-17"

from collections import defaultdict, deque
from functools import partial
from itertools import count
from operator import itemgetter
from sys import stdin

def make_runner(program, in_, out_):
    program_ = defaultdict(lambda: 0, enumerate(program))
    position, program, halted, stalled, rel_base = 0, program_, False, False, 0
    while not halted:
        state = position, program, halted, stalled, rel_base, in_, out_
        position, program, halted, stalled, rel_base, in_, out_ = step(state)
        if stalled:
            yield halted, stalled, program
    yield halted, stalled, program

fid = lambda x: x
eval = lambda p, r, m, v, rd=True:\
    (p.__getitem__ if rd and m != 1 else fid)(v if m < 2 else r + v)

def decode(code):
    yield code % 100
    yield from (code // (10**i) % 10 for i in count(start=2))

def read(program, position):
    yield from (program[i] for i in count(start=position + 1))

def step(state):
    position, program, halted, stalled, rel_base, in_, out_ = state
    control = iter(decode(program[position]))
    operation, vs = next(control), zip(control, read(program, position))
    e = partial(eval, program, rel_base)
    if operation == 1:
        program[e(*next(vs), False)] = e(*next(vs)) + e(*next(vs))
        position += 4
    elif operation == 2:
        program[e(*next(vs), False)] = e(*next(vs)) * e(*next(vs))
        position += 4
    elif operation == 3 and in_:
        program[e(*next(vs), False)] = in_.popleft()
        position += 2
    elif operation == 4:
        out_.append(e(*next(vs)))
        position += 2
    elif operation == 5:
        if e(*next(vs)):
            position = e(*next(vs))
        else:
            position += 3
    elif operation == 6:
        if not e(*next(vs)):
            position = e(*next(vs))
        else:
            position += 3
    elif operation == 7:
        program[e(*next(vs), False)] = e(*next(vs)) < e(*next(vs))
        position += 4
    elif operation == 8:
        program[e(*next(vs), False)] = e(*next(vs)) == e(*next(vs))
        position += 4
    elif operation == 9:
        rel_base += e(*next(vs))
        position += 2
    elif operation == 99:
        position += 1
        halted = True
    else:
        stalled = True
    return position, program, halted, stalled, rel_base, in_, out_

program = [int(line) for line in stdin.read().split(',')]
out = deque()
next(make_runner(program, deque([1]), out))
print(f'1: {out.pop()}')

next(make_runner(program, deque([2]), out))
print(f'2: {out.pop()}')