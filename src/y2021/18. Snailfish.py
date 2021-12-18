#!/usr/bin/env python3

"""
AoC Day 18 - Snailfish - in Python.
"""

__author__ = "Lucas Moeskops"
__date__ = "2021-18-01"

from itertools import permutations
from math import ceil
from sys import argv, stdin

from helpers import timed

lines = stdin.read().strip().split('\n')


def prop_left(p, value):
    if isinstance(p, int):
        return p + value
    return [prop_left(p[0], value), p[1]]


def prop_right(p, value):
    if isinstance(p, int):
        return p + value
    return [p[0], prop_right(p[1], value)]


did_explode = False
def do_explode(p, depth=0):
    global did_explode
    if isinstance(p, int):
        return p
    if depth >= 3 and not did_explode:
        for i, x in enumerate(p):
            if isinstance(x, list) and isinstance(x[0], int) and isinstance(x[1], int):
                l, r = x
                app_l, app_r = False, False
                if i > 0:
                    p[i-1] = prop_right(p[i-1], l)
                    app_l = True
                if i < len(p) - 1:
                    p[i+1] = prop_left(p[i+1], r)
                    app_r = True
                p[i] = 0
                did_explode = True
                return (p, (l if not app_l else None, r if not app_r else None))
    app_l, app_r = True, True
    l, r = 0, 0
    for i, x in enumerate(p):
        p[i] = do_explode(x, depth=depth+1)
        if isinstance(p[i], tuple):
            p[i], (l, r) = p[i]
            if l is not None:
                if i > 0:
                    p[i-1] = prop_right(p[i-1], l)
                app_l = i > 0
            if r is not None:
                if i < len(p) - 1:
                    p[i+1] = prop_left(p[i+1], r)
                app_r = i < len(p) - 1
            break
    return p if app_l and app_r else (p, (l if not app_l else None, (r if not app_r else None)))


did_split = False
def do_split(p, depth=0):
    global did_split
    if not isinstance(p, int):
        return [do_split(x, depth=depth+1) for x in p]
    if p >= 10 and not did_split:
        did_split = True
        return [int(p / 2), ceil(p / 2)]
    return p


def find_magnitude(p):
    if isinstance(p, int):
        return p
    return 3 * find_magnitude(p[0]) + 2 * find_magnitude(p[1])


def reduce(problems):
    global did_explode, did_split
    problem = None
    for i, x in enumerate(problems):
        problem = [problem, eval(x)] if problem else eval(x)
        init = True
        while init or did_explode or did_split:
            init = False
            did_explode = False
            problem = do_explode(problem)
            if isinstance(problem, tuple):
                problem = problem[0]
            if did_explode:
                continue
            did_split = False
            problem = do_split(problem)
    return problem

@timed
def task_1():
    return find_magnitude(reduce(lines))


@timed
def task_2():
    return max(map(find_magnitude, map(reduce, permutations(lines, 2))))


print(f'[Part 1]: {task_1()}')
print(f'[Part 2]: {task_2()}')