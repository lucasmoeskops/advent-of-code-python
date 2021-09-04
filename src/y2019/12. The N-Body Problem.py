"""
AoC Day 12 - The N-Body Problem.
"""

__author__ = "Lucas Moeskops"
__date__ = "2021-02-07"

from functools import reduce
from itertools import islice
from operator import itemgetter
from math import gcd, prod
from sys import stdin
from typing import NamedTuple

class Moon(NamedTuple):
    x: int
    y: int
    z: int
    vx: int = 0
    vy: int = 0
    vz: int = 0


def apply_gravity(moons):
    for i, (x, y, z, vx, vy, vz) in enumerate(moons):
        for j, (ox, oy, oz, _, _, _) in enumerate(moons):
            if i == j:
                continue
            vx += (x < ox) - (x > ox)
            vy += (y < oy) - (y > oy)
            vz += (z < oz) - (z > oz)
        yield Moon(x, y, z, vx, vy, vz)

def apply_velocity(moons):
    for x, y, z, vx, vy, vz in moons:
        yield Moon(x + vx, y + vy, z + vz, vx, vy, vz)

def state(moons):
    while True:
        yield moons
        moons = list(apply_gravity(moons))
        moons = list(apply_velocity(moons))

def potential_energy(moon):
    x, y, z, _, _, _ = moon
    return sum(map(abs, (x, y, z)))

def kinetic_energy(moon):
    _, _, _, vx, vy, vz = moon
    return sum(map(abs, (vx, vy, vz)))

def total_energy(moons):
    return sum(potential_energy(moon) * kinetic_energy(moon) for moon in moons)

moons_raw = stdin.read().split('\n')
moons_raw = (reduce(lambda s, c: s.replace(c, ''), '<> ', moon) for moon in moons_raw)
moons_raw = (moon.split(',') for moon in moons_raw)
moons = [Moon(*(int(value.split('=')[1]) for value in moon)) for moon in moons_raw]
moons_after_1000 = next(islice(state(moons), 1000, 1001))

print(f'1: {total_energy(moons_after_1000)}')

def apply_gravity_single(moon_aspects):
    for i, (x, vx) in enumerate(moon_aspects):
        for j, (ox, _) in enumerate(moon_aspects):
            if i == j:
                continue
            vx += (x < ox) - (x > ox)
        yield x, vx

def apply_velocity_single(moon_aspects):
    for x, vx in moon_aspects:
        yield x + vx, vx

def iteration_count_per_dimension(moons):
    for dimension in range(3):
        getter = itemgetter(dimension, dimension + 3)
        moon_aspects = list(map(getter, moons))
        initial = moon_aspects[:]
        iterations = 0

        while any(map(itemgetter(1), moon_aspects)) or not iterations:
            moon_aspects = list(apply_gravity_single(moon_aspects))
            moon_aspects = list(apply_velocity_single(moon_aspects))
            iterations += 1
            
        yield iterations * 2

def find_cycle_length(moons):
    counts = list(iteration_count_per_dimension(moons))
    divider = gcd(*counts)
    return prod(count // divider for count in counts)

print(f'2: {find_cycle_length(moons)}')
