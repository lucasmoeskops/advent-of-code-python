#!/usr/bin/env python3

"""
AoC Day 6 - Custom Customs - in Python.

With sets.
"""

__author__ = "Lucas Moeskops"
__date__ = "2020-12-06"

from functools import reduce
from sys import stdin

questions_any = lambda group: len(set(''.join(group.replace('\n', ''))))

questions_all = lambda group: len(reduce(set.__and__, map(set, group.split('\n'))))

groups = stdin.read().split('\n\n')

print(f'1: {sum(map(questions_any, groups))}')
print(f'2: {sum(map(questions_all, groups))}')
