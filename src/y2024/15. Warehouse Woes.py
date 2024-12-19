#!/usr/bin/env python3

"""
AoC Day 15 - Warehouse Woes - in Python.
"""

__author__ = "Lucas Moeskops"
__date__ = "2024-12-15"
__summary__ = "Back to year 2021, day 6"

from lib import *


class WallError(Exception):
    pass


def find_moves(dx, dy, bx, by):
    moves = set()

    if plan2[by][bx] == '#':
        raise WallError()

    elif plan2[by][bx] == '.':
        return moves

    elif plan2[by][bx] == '[':
        db = 1
    else:
        db = -1

    moves.add((bx, by))
    moves.add((bx + db, by))

    if (bx + dx, by + dy) not in moves:
        moves.update(find_moves(dx, dy, bx + dx, by + dy))

    if (bx + db + dx, by + dy) not in moves:
        moves.update(find_moves(dx, dy, bx + db + dx, by + dy))

    return moves


puzzle = read_input(2024, 15)
plan, movements = puzzle.split('\n\n')
width = plan.index('\n')
height = plan.count('\n') + 1
plan = plan.replace('\n', '')
plan2 = plan.replace('#', '##').replace('O', '[]').replace('.', '..').replace('@', '@.')
movements = movements.replace('\n', '')
plan = list(map(list, batched(plan, width)))
plan2 = list(map(list, batched(plan2, width * 2)))

for plan, width, height in ((plan, width, height), (plan2, width * 2, height)):
    x, y = [(x, y) for y, row in enumerate(plan) for x, v in enumerate(row) if v == '@'][0]
    for movement in movements:
        dx, dy = (0, 1) if movement == 'v' else (0, -1) if movement == '^' else (1, 0) if movement == '>' else (-1, 0)
        nx, ny = x + dx, y + dy
        value = plan[ny][nx]

        if value == '#':
            continue

        elif value == '.':
            plan[ny][nx] = '@'
            plan[y][x] = '.'
            x, y = nx, ny

        elif value == 'O':
            bx, by = nx, ny
            moves = [(bx, by)]

            while True:
                bx, by = bx + dx, by + dy

                if plan[by][bx] == '#':
                    moves = []
                    break

                elif plan[by][bx] == '.':
                    break

                moves.append((bx, by))

            if not moves:
                continue

        else:
            try:
                moves = list(find_moves(dx, dy, nx, ny))
            except WallError:
                continue

            if movement == '>':
                moves.sort(key=itemgetter(0))
            elif movement == '<':
                moves.sort(key=itemgetter(0), reverse=True)
            elif movement == 'v':
                moves.sort(key=itemgetter(1))
            elif movement == '^':
                moves.sort(key=itemgetter(1), reverse=True)

        while moves:
            bx, by = moves.pop()
            plan[by+dy][bx+dx] = plan[by][bx]
            plan[by][bx] = '.'

        plan[ny][nx] = '@'
        plan[y][x] = '.'
        x, y = nx, ny

    print(sum(y * 100 + x for y, row in enumerate(plan) for x, v in enumerate(row) if v == 'O' or v == '['))
