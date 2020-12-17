#!/usr/bin/env python3

"""
AoC Day 17 - Conway Cubes - in Python.

Generic dimension number implementation for 2+ dimensions.
Completes in about 18 seconds on my laptop.
"""

__author__ = "Lucas Moeskops"
__date__

from itertools import product
from sys import stdin

vector_add = lambda t, u: tuple(a + b for (a, b) in zip(t, u))

num_neighbours = (lambda cubes, p:
    sum(1 for o in product(range(-1, 2), repeat=len(p))
        if vector_add(p, o) in cubes and any(d != 0 for d in o)))

active_sum = lambda cubes: sum(1 for c in cubes)

def run(dim, num_rounds, lines):
    w, h, cubes, g = *new_state(dim, lines), range
    for r in range(1, num_rounds + 1):
        ac, inac, newtest = set(), set(), set()
        for p in product(range(-r, w+r), range(-r, h+r), *[[*range(-r, r+1)]] * (dim-2)):
            active = p in cubes
            n = num_neighbours(cubes, p)
            if active and (n < 2 or 3 < n):
                inac.add(p)
            elif n == 3:
                ac.add(p)
        cubes = (cubes - inac) | ac
    return cubes

new_state = (lambda d, l:
    (len(l[0] if l else 0),
     len(l),
     {(x, y, *[0] * (d-2)) for y, r in enumerate(l) for x, c in enumerate(r)
      if c == '#'}))

lines = stdin.read().split('\n')

print(f'1: {active_sum(run(3, 6, lines))}')
print(f'2: {active_sum(run(4, 6, lines))}')