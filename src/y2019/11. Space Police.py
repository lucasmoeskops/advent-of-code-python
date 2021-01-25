"""
AoC Day 11 - Space Police.
"""

__author__ = "Lucas Moeskops"
__date__ = "2021-01-25"

from collections import defaultdict, deque
from functools import partial
from itertools import count, tee
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

def run(program, initial=0):
    halted, in_, out_, position = False, deque(), deque(), (0, 0)
    runner = make_runner(program, in_, out_)
    data = {position: initial}
    direction = 0
    while not halted:
        in_.append(data.get(position, 0))
        halted, *_ = next(runner)
        if not halted:
            data[position] = out_.popleft()
            direction = (direction + (1 if out_.popleft() else -1)) % 4
            position =\
                position[0] + (direction % 2) * (-direction + 2),\
                position[1] + (not direction % 2) * (direction - 1),
    return data


program = [int(line) for line in stdin.read().split(',')]
print(f'1: {len(run(program))}')

def format_row(data, sx, ex, y):
    return ''.join(str(data.get((x, y), 0)) for x in range(sx, ex + 1))

data = run(program, initial=1)
xs, ys = tee(map(itemgetter(0), data), 2), tee(map(itemgetter(1), data), 2)
sx, ex, sy, ey = min(xs[0]), max(xs[1]), min(ys[0]), max(ys[1])
output = '\n'.join(map(partial(format_row, data, sx, ex), range(sy, ey + 1)))
print(f'2: \n{output.replace("0", " ")}')
