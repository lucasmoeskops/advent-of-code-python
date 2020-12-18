#!/usr/bin/env python3

"""
AoC Day 18 - Operation Order - in Python.

Solved with repeated regex substitutions :-)
"""

__author__ = "Lucas Moeskops"
__date__ = "2020-12-18"

from re import sub
from sys import stdin


def math(op):
    while '(' in op:
        op = sub(r'\(([^(-)]+)\)', lambda m: math(m.groups()[0]), op)
    return str(eval(op.count(' ') // 2 * '(' + sub(r'([+*])', ')\g<1>', op)))

def advanced_math(op):
    while '(' in op:
        op = sub(r'\(([^(-)]+)\)', lambda m: advanced_math(m.groups()[0]), op)
    return str(eval(sub(r'(\d+( \+ \d+)+)', '(\g<1>)', op)))

lines = stdin.read().split('\n')

print(f'1: {sum(int(math(line)) for line in lines)}')
print(f'2: {sum(int(advanced_math(line)) for line in lines)}')