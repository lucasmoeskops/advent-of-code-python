#!/usr/bin/env python3

"""
AoC Day 14 - Extended Polymerization - in Python.
"""

__author__ = "Lucas Moeskops"
__date__ = "2021-12-14"

from collections import Counter
from itertools import pairwise, count, dropwhile
from operator import itemgetter

from sys import stdin

from helpers import timed, parse_from_re

part1, part2 = stdin.read().split('\n\n')
template = part1.strip()
lines = part2.split('\n')
rule_re = r'(?P<a>\w)(?P<b>\w) -> (?P<c>\w)'
rules = parse_from_re(rule_re, {}, lines)
g = None


def generator():
    patterns = [a + b for a, b in pairwise(template)]
    counter = Counter(patterns)
    simple_rules = list(map(itemgetter('a', 'b', 'c'), rules))
    for step in count(start=1):
        new_counts = Counter()
        for a, b, c in simple_rules:
            ab, ac, cb = a + b, a + c, c + b
            if ab_num := counter[ab]:
                new_counts[ac] += ab_num
                new_counts[cb] += ab_num
                new_counts[ab] -= ab_num
        counter.update(new_counts)
        yield step, counter


def count_difference_between_most_common_and_least_common(pattern_counter):
    char_counter = Counter(template[-1])
    for p, v in pattern_counter.items():
        char_counter[p[0]] += v
    most_common = char_counter.most_common()
    return most_common[0][1] - most_common[-1][1]


@timed
def task_1():
    global g
    g = g or generator()
    _, pattern_counter = next(dropwhile(lambda r: r[0] < 10, g))
    return count_difference_between_most_common_and_least_common(pattern_counter)


@timed
def task_2():
    global g
    g = g or generator()
    _, pattern_counter = next(dropwhile(lambda r: r[0] < 40, g))
    return count_difference_between_most_common_and_least_common(pattern_counter)


print(f'[Part 1]: {task_1()}')
print(f'[Part 2]: {task_2()}')
