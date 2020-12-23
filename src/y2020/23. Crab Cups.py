#!/usr/bin/env python3

"""
AoC Day 23 - Crab Cups - in Python.

One function to rule them all.
"""

__author__ = "Lucas Moeskops"
__date__ = "2020-12-23"

from itertools import islice
from math import prod
from sys import stdin

decode = lambda i: i + 1
encode = lambda i: i - 1

def iterator_from(state, n):
    for _ in range(len(state)):
        n = state[n]
        yield n

def play(initial, total=0, moves=100):
    total = max(total, len(initial))
    state = [i + 1 for i in range(total)]
    state[-1] = initial[0]
    for i, v in enumerate(initial[1:]):
        state[initial[i]] = v
    if total > len(initial):
        state[initial[len(initial) - 1]] = len(initial)
    current = initial[0]
    for _ in range(moves):
        a = state[current]
        b = state[a]
        c = state[b]
        state[current] = state[c]
        dest = (current - 1) % total
        while dest == a or dest == b or dest == c:
            dest = (dest - 1) % total
        state[c] = state[dest]
        state[dest] = a
        current = state[current]
    return state

initial = [encode(int(i)) for i in stdin.read()]

print(f'1: {"".join(str(decode(v)) for v in iterator_from(play(initial), 0))}')

result = iterator_from(play(initial, 1000000, 10000000), 0)
print(f'2: {prod(decode(v) for v in islice(result, 2))}')