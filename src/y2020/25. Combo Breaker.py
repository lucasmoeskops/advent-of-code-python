#!/usr/bin/env python3

"""
AoC Day 25 - Combo Breaker - in Python.

With a nice O(sqrt(N)) speed-up algorithm.
"""

__author__ = "Lucas Moeskops"
__date__ = "2020-12-25"

from math import ceil, sqrt
from sys import stdin

modulo = 20201227

def babystep_giantstep(g, h, mod):
    # Source: https://en.wikipedia.org/wiki/Baby-step_giant-step
    m, table, e = ceil(sqrt(mod)), {}, 1
    for i in range(m):
        table[e] = i
        e = e * g % mod
    factor, e = pow(g, mod - m - 1, mod), h
    for i in range(m):
        if e in table:
            return i * m + table[e]
        e = e * factor % mod
    return

lines = stdin.read().split('\n')
card_pk, door_pk = map(int, lines)

loop_size = babystep_giantstep(7, card_pk, modulo)

print(f'1: {pow(door_pk, loop_size, modulo)}')
print(f'2: Merry Christmas')