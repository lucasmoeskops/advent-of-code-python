#!/usr/bin/env python3

"""
AoC Day 7 - Handy Haversacks - in Python.

Some random tree methods to solve the problem.
"""

__author__ = "Lucas Moeskops"
__date__ = "2020-12-07"

from collections import defaultdict
from functools import reduce
from sys import stdin

def parse_rule(s):
    name, contents = s.replace('.', '').replace('bags', 'bag').split(' contain ')
    parse_item = lambda i: (int(i[0].replace('no', '0')), i[1])
    items = [parse_item(i.split(' ', 1)) for i in contents.split(', ')]
    return name, items

def reverse_rules(rules):
    m = defaultdict(list)
    for n, inners in rules.items():
        for num, g in inners:
            m[g].append((num, n))
    return m

parents = lambda reverse_rules, child: [name for num, name in reverse_rules[child]]

ancestors = (lambda reverse_rules, child:
    reduce(set.__or__,
           [{parent, *ancestors(reverse_rules, parent)}
            for parent in parents(reverse_rules, child)],
           set()))

num_descendants = (lambda rules, name:
    sum([num + num_descendants(rules, name_) * num
         for num, name_ in rules[name] if num > 0]))

lines = stdin.read().split('\n')
rules = dict(map(parse_rule, lines))

print(f'1: {len(ancestors(reverse_rules(rules), "shiny gold bag"))}')
print(f'2: {num_descendants(rules, "shiny gold bag")}')
