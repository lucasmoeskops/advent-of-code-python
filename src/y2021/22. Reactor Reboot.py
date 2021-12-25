#!/usr/bin/env python3

"""
AoC Day 22 - Reactor Reboot - in Python.
"""

__author__ = "Lucas Moeskops"
__date__ = "2021-12-22"

from itertools import product
from operator import itemgetter
from sys import stdin, stdout

from helpers import parse_from_re, timed


line_re = r'(?P<action>(on)|(off)) x=(?P<sx>[\w-]+)..(?P<ex>[\w-]+),y=(?P<sy>[\w-]+)..(?P<ey>[\w-]+),z=(?P<sz>[\w-]+)..(?P<ez>[\w-]+)'
mapper = {v: int for v in ('sx', 'sy', 'sz', 'ex', 'ey', 'ez')}
rows = parse_from_re(line_re, mapper, stdin.read().strip().split('\n'))
rows = list(map(itemgetter('action', 'sx', 'ex', 'sy', 'ey', 'sz', 'ez'), rows))


@timed
def task_1():
    total_on = 0
    useful_rows = [
        (action, sx, ex, sy, ey, sz, ez)
        for action, sx, ex, sy, ey, sz, ez in rows
        if sx <= 50 and ex >= -50 and sy <= 50 and ey >= -50 and sz <= 50 and ez >= -50
    ]
    for z in range(-50, 51):
        for y in range(-50, 51):
            for x in range(-50, 51):
                for action, sx, ex, sy, ey, sz, ez in reversed(useful_rows):
                    if sx <= x <= ex and sy <= y <= ey and sz <= z <= ez:
                        total_on += 1 if action == 'on' else 0
                        break
    return total_on


def is_valid_cube(cube, bounding_cube):
    action, sx, ex, sy, ey, sz, ez = cube
    _, bsx, bex, bsy, bey, bsz, bez = bounding_cube
    return sx <= ex and sy <= ey and sz <= ez and sx >= bsx and ex <= bex and sy >= bsy and ey <= bey and sz >= bsz and ez <= bez


def overlap(cube, other_cube):
    action, sx, ex, sy, ey, sz, ez = cube
    _, osx, oex, osy, oey, osz, oez = other_cube
    return not (sx > oex or ex < osx or sy > oey or ey < osy or sz > oez or ez < osz)


def subtract_cube(cube, other_cube):
    action, sx, ex, sy, ey, sz, ez = cube
    _, osx, oex, osy, oey, osz, oez = other_cube
    bsx = max(sx, min(ex+1, osx))
    bex = min(ex, max(sx-1, oex))
    bsy = max(sy, min(ey+1, osy))
    bey = min(ey, max(sy-1, oey))
    bsz = max(sz, min(ez+1, osz))
    bez = min(ez, max(sz-1, oez))

    if sx > oex or ex < osx or sy > oey or ey < osy or sz > oez or ez < osz:
        # Optimalisation
        return [cube]

    new_cubes = [
        (
            action,
            [sx, bsx, bex + 1][a],
            [bsx - 1, bex, ex][a],
            [sy, bsy, bey + 1][b],
            [bsy - 1, bey, ey][b],
            [sz, bsz, bez + 1][c],
            [bsz - 1, bez, ez][c],
        )
        for a, b, c in product(range(3), repeat=3)
    ]
    return [cube2 for cube2 in new_cubes if is_valid_cube(cube2, cube) and not is_valid_cube(cube2, other_cube)]


def size(cube):
    action, sx, ex, sy, ey, sz, ez = cube
    return (ex - sx + 1) * (ey - sy + 1) * (ez - sz + 1)


@timed
def task_2():
    total_on = 0
    used = []
    for i, cube in enumerate(reversed(rows)):
        cubes = [cube]
        if cube[0] == 'on':
            for other_cube in sorted(used, key=lambda oc: len(subtract_cube(cube, oc))):
                cubes = [cube for current_cube in cubes for cube in subtract_cube(current_cube, other_cube)]
            total_on += sum(size(cube) for cube in cubes)
        print(f':{i}', end='')
        stdout.flush()
        used.append(cube)
    return total_on


print(f'[Part 1]: {task_1()}')
print(f'[Part 2]: {task_2()}')