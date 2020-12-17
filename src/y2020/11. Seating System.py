#!/usr/bin/env python3

"""
AoC Day 11 - Seating System - in Python.

Conway-like grid. Some optimizations for faster solving.
"""

__author__ = "Lucas Moeskops"
__date__ = "2020-12-11"

from collections import defaultdict
from sys import stdin

def neighbour_tracer(state):
    w, h, seats = state
    mem = [1 for _ in seats]
    for i, char in enumerate(seats):
        if char == '.':
            continue
        x, y = i % w, i // w
        fields = ((x+dx, y+dy) for (dx, dy) in directions)
        fields = (x + w * y for (x, y) in fields if 0 <= x < w and 0 <= y < h)
        mem[i] = tuple(j for j in fields if seats[j] != '.')
    return mem

def ray_tracer(state):
    w, h, seats = state
    mem = [[] for _ in seats]
    for i, char in enumerate(seats):
        if char == '.':
            continue
        x, y = i % w, i // w
        for dx, dy in directions2:
            p, q = x, y
            while True:
                p += dx
                q += dy
                if not (0 <= p < w and 0 <= q < h):
                    break
                j = p + q * w
                if seats[j] in ('L', '#'):
                    mem[i].append(j)
                    mem[j].append(i)
                    break
    return mem

def round(determinator, tolerance, state):
    w, h, seats, changes = *state, []
    for i, char in enumerate(seats):
        if char == '.':
            continue
        xs = [seats[j] for j in determinator[i]]
        if char == 'L' and all(x != '#' for x in xs):
            changes.append((i, '#'))
        elif char == '#' and len(xs) >= tolerance <= sum(1 for x in xs if x == '#'):
            changes.append((i, 'L'))
    for i, c in changes:
        seats[i] = c
    return bool(changes)

def simulate(determinator, tolerancy, state):
    m = determinator(state)
    while round(m, tolerancy, state):
        pass
    return sum(1 for char in state[2] if char == '#')

new_state = (lambda lines:
    (len(lines[0] if lines else 0),
     len(lines),
     [char for line in lines for char in line]))

directions = [(dx, dy) for dy in range(-1, 2) for dx in range(-1, 2) if dy or dx]
directions2 = [(1, -1), (1, 0), (1, 1), (0, 1)]

lines = stdin.read().split('\n')

print(f'1: {simulate(neighbour_tracer, 4, new_state(lines))}')
print(f'2: {simulate(ray_tracer, 5, new_state(lines))}')