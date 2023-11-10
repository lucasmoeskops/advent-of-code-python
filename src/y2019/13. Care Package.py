"""
AoC Day 13 - Care Package.
"""

__author__ = "Lucas Moeskops"
__date__ = "2021-09-04"
__summary__ = "Receive pinball game from elves to keep you sane"

from lib import *
from sys import stdin
from y2019.computer import make_runner


class Tile(Enum):
    EMPTY = 0
    WALL = 1
    BLOCK = 2
    PADDLE = 3
    BALL = 4


program = [int(code) for code in stdin.read().split(',')]
out = []
next(make_runner(program, [], out))
num_block_tiles = sum(1 for tile in out[2::3] if tile == Tile.BLOCK.value)
print(num_block_tiles)


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
print(play(program))
