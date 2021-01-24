"""
AoC Day 10 - Monitoring Station - in Python.
"""

__author__ = "Lucas Moeskops"
__date__ = "2021-01-24"

from collections import defaultdict
from functools import partial
from math import atan2, gcd, pi
from sys import stdin

def normalize(x, y):
    d = gcd(x, y)
    return x // d, y // d

def relative_position(x1, y1, x2, y2):
    return x2 - x1, y2 - y1

def asteroid_groups(asteroids, point):
    f = partial(relative_position, *point)
    d = defaultdict(list)
    for a in asteroids:
        if a == point:
            continue
        d[normalize(*f(*a))].append(a)
    return d

def sight_count(*args):
    return len(asteroid_groups(*args))

field = stdin.read()
width = field.index('\n') + 1  # including newline
asteroids = [(i % width, i // width) for i, c in enumerate(field) if c == '#']

print(f'1: {max(map(partial(sight_count, asteroids), asteroids))}')

def rotation_order(x, y):
    return (pi - atan2(x, y))

best_asteroid = max(asteroids, key=partial(sight_count, asteroids))
groups = asteroid_groups(asteroids, best_asteroid)
distance = partial(relative_position, *best_asteroid)
# Assumption here: 200 is less than the solution of part 1
# If not we would have to discard sets of closest items until discarding another
# layer would discard the 200th and continue with the remaining items instead.
closest = [min(g, key=lambda p: sum(distance(*p))) for g in groups.values()]
target_asteroid = sorted(closest, key=lambda p: rotation_order(*distance(*p)))[199]

print(f'2: {target_asteroid[0] * 100 + target_asteroid[1]}')
