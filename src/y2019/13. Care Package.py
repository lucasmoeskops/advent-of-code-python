"""
AoC Day 13 - Care Package.
"""

__author__ = "Lucas Moeskops"
__date__ = "2021-09-04"

from collections import defaultdict, deque
from enum import Enum
from functools import partial
from itertools import count
from sys import stdin

class Tile(Enum):
    EMPTY = 0
    WALL = 1
    BLOCK = 2
    PADDLE = 3
    BALL = 4

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
    stalled = False
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

program = [int(code) for code in stdin.read().split(',')]
out = []
next(make_runner(program, [], out))
num_block_tiles = sum(1 for tile in out[2::3] if tile == Tile.BLOCK.value)
print(f'1: {num_block_tiles}')

def play(program):
    halted, in_, out_, score, ball_x, paddle_x = False, deque(), deque(), 0, 0, 0
    program = make_runner(program, in_, out_)
    halted, *_ = next(program)
    
    while True:
        while out_:
            x, y, t = out_.popleft(), out_.popleft(), out_.popleft()

            if x == -1:
                score = t
                continue
            
            if Tile(t) == Tile.BALL:
                ball_x = x
            elif Tile(t) == Tile.PADDLE:
                paddle_x = x

        if not halted:
            in_.append((paddle_x < ball_x) - (paddle_x > ball_x))
            halted, *_ = next(program)
        else:
            break

    return score

program[0] = 2
print(f'2: {play(program)}')