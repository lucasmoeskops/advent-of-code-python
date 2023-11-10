"""
AoC Day 11 - Space Police.
"""

__author__ = "Lucas Moeskops"
__date__ = "2021-01-25"
__summary__ = "Repaint ship number for Space Police near Jupiter"

from lib import *
from sys import stdin
from y2019.computer import make_runner


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


def format_row(data, sx, ex, y):
    return ''.join(str(data.get((x, y), 0)) for x in range(sx, ex + 1))


program = [int(line) for line in stdin.read().split(',')]
print(len(run(program)))

data = run(program, initial=1)
xs, ys = tee(map(itemgetter(0), data), 2), tee(map(itemgetter(1), data), 2)
sx, ex, sy, ey = min(xs[0]), max(xs[1]), min(ys[0]), max(ys[1])
output = '\n'.join(map(partial(format_row, data, sx, ex), range(sy, ey + 1)))
print(output.replace("0", ".").replace("1", "#"))
