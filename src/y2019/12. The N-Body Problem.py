"""
AoC Day 12 - The N-Body Problem.
"""

__author__ = "Lucas Moeskops"
__date__ = "2021-02-07"
__summary__ = "Determine movements of Jupiter's moons and ponder"

from lib import *
from sys import stdin


def transform(state):
    h = len(state[0]) // 2
    empty = [0] * h

    # Apply gravity
    for moon in state:
        for other_moon in state:
            moon.add(Vector(*empty, *(Vector(*other_moon[:h]) - Vector(*moon[:h])).sign()))

    # Add velocity
    for moon in state:
        moon.add(Vector(*moon[h:], *empty))

    return state


def all_velocities_zero(single_dimensions):
    return not any(map(itemgetter(1), single_dimensions))


moons_raw = stdin.read().split('\n')
moons = [Vector(*ints(moon), 0, 0, 0) for moon in moons_raw]
moons_after_1000 = nth(transform_stream(transform, deepcopy(moons)), 1000)
print(sum(sum(abs(moon)[:3]) * sum(abs(moon)[3:]) for moon in moons_after_1000))

periods = []

for i in range(3):
    _, rounds = until(transform, [Vector(m[i], m[i+3]) for m in moons], select(all_velocities_zero, 0))
    periods.append((rounds + 1) * 2)

print(reduce(lambda p, q: p * q // gcd(p, q), periods))
