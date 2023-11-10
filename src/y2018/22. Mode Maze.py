from lib import *
from sys import stdin


ROCKY = 0
WET = 1
NARROW = 2

NEITHER = 0
TORCH = 1
CLIMBING_GEAR = 2


@cache
def erosion_level_at(x, y):
    return (geologic_index_at(x, y) + depth) % 20183


def geologic_index_at(x, y):
    if x == ex and y == ey:
        return 0
    elif x == 0:
        return y * 48271
    elif y == 0:
        return x * 16807

    return erosion_level_at(x - 1, y) * erosion_level_at(x, y - 1)


def condition_at(x, y):
    match erosion_level_at(x, y) % 3:
        case 0:
            return ROCKY
        case 1:
            return WET
        case 2:
            return NARROW


def valid_gear(condition):
    if condition == ROCKY:
        return CLIMBING_GEAR, TORCH
    elif condition == WET:
        return CLIMBING_GEAR, NEITHER
    elif condition == NARROW:
        return TORCH, NEITHER


def get_options(x, y, gear):
    for nx, ny in neighbors2d4(x, y):
        if nx < 0 or ny < 0:
            continue

        if gear in valid_gear(condition_at(nx, ny)):
            yield 1, (nx, ny, gear)

    for other_gear in (CLIMBING_GEAR, TORCH, NEITHER):
        if gear != other_gear and other_gear in valid_gear(condition_at(x, y)):
            yield 7, (x, y, other_gear)


def target_reached(x, y, gear):
    return x == ex and y == ey and gear == TORCH


def heuristic(x, y, gear):
    return abs(x - ex) + abs(y - ey) + (0 if gear == TORCH else 7)


depth, ex, ey = ints(stdin.read())

print(sum(starmap(condition_at, range2d(0, 0, ex + 1, ey + 1))))
path = astar([(0, 0, TORCH)], curry(get_options), curry(target_reached), curry(heuristic))
print(path[0])
