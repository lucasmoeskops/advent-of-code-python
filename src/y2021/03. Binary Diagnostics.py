#!/usr/bin/env python3

"""
AoC Day 3 - Binary Diagnostic - in Python.
"""

__author__ = "Lucas Moeskops"
__date__ = "2021-12-03"


from sys import stdin


LINES = stdin.read().split('\n')


def part_1():
    gamma_rate = ''.join(max('01', key=p.count) for p in zip(*LINES))
    epsilon_rate = gamma_rate.translate(''.maketrans(*'01'))
    return int(gamma_rate, 2) * int(epsilon_rate, 2)


def find_match(_filter_, order, candidates):
    def inner(cs, i=0):
        if len(cs) == 1:
            return cs[0]
        col = [c[i] for c in cs]
        target = _filter_([c for c in order if c in col], key=col.count)
        cs = [c for c in cs if c[i] == target]
        return inner(cs, i + 1)
    return inner(candidates)


def part_2():
    oxygen_generator_rating = find_match(max, '10', LINES)
    co2_scrubber_rating = find_match(min, '01', LINES)
    return int(oxygen_generator_rating, 2) * int(co2_scrubber_rating, 2)


print(part_1())
print(part_2())
