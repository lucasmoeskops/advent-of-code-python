#!/usr/bin/env python3

"""
AoC Day 14 - Docking Data - in Python.

Solver that only operators on integer values, using a nice
fast bitwise permutation generator algorithm learned from Voltara.
"""

__author__ = "Lucas Moeskops"
__date__ = "2020-12-14"

from functools import reduce
from re import match
from sys import stdin

make_mask = (lambda op, c, m, s:
    reduce(op, (2**i for i, v in enumerate(reversed(m)) if v == c), s))

add_mask      = lambda c, m: make_mask(int.__add__, c, m, 0)
subtract_mask = lambda c, m: make_mask(int.__sub__, c, m, (2<<35) - 1)

def permutations(mask):
    # bit permutation-trick with credits to askalski/Voltara
    n = -mask & mask
    yield 0
    while n != 0:
        yield n
        n = (n - mask) & mask

def decode(lines, version=1):
    mem = {}
    for line in lines:
        m = match(r'mem\[(\d+)\] = (\d+)', line)
        if m:
            address, value = map(int, m.groups())
            if version == 1:
                mem[address] = value & and_mask | or_mask
            else:
                address = address & and_mask | or_mask
                for option in permutations(x_mask):
                    mem[address + option] = value
        else:
            mask = line.split(' = ')[1]
            or_mask = add_mask('1', mask)
            if version == 1:
                and_mask = subtract_mask('0', mask)
            else:
                x_mask = add_mask('X', mask)
                and_mask = subtract_mask('X', mask)
    return sum(mem.values())

lines = stdin.read().split('\n')

print(f'1: {decode(lines)}')
print(f'2: {decode(lines, 2)}')