from collections import defaultdict
from itertools import cycle, islice, count, tee
from math import prod
from sys import stdin

DATA = stdin.read()

N, NE, E, SE, S, SW, W, NW = range(8)
CONSIDER_ORDER = N, S, W, E

DIRECTIONS = dict(enumerate(
    (lambda a, b: zip(a, islice(b, 6, 14)))(*tee(cycle([0, 1, 1, 1, 0, -1, -1, -1])))
))  # For fun


def round(positions, considerings):
    considered_moves = defaultdict(list)
    considerors = set()

    for x, y in positions:
        for dx, dy in DIRECTIONS.values():
            if (x + dx, y + dy) in positions:
                considerors.add((x, y))
                break

    if not considerors:
        return True

    for direction in islice(considerings, 4):
        check = tuple(map(
            DIRECTIONS.__getitem__,
            islice(cycle(range(8)), direction + 7, direction + 10)
        ))

        for x, y in tuple(considerors):
            for dx, dy in check:
                if (x + dx, y + dy) in positions:
                    break
            else:
                # can move
                considered_moves[(x + check[1][0], y + check[1][1])].append((x, y))
                considerors.remove((x, y))

    for location, elves in considered_moves.items():
        if len(elves) == 1:
            positions.remove(elves[0])
            positions.add(location)

    next(considerings)
    return False


def do():
    considerings = cycle(CONSIDER_ORDER)
    positions = {
        (x, y)
        for y, row in enumerate(DATA.split('\n'))
        for x, val in enumerate(row) if val == '#'
    }

    for number in count(start=1):
        if round(positions, considerings):
            print(number)  # part 2
            break

        if number == 10:
            print(
                prod(
                    max(vs) - min(vs) + 1
                    for vs in ([p[d] for p in positions] for d in range(2))
                ) - len(positions)
            )  # part 1


do()
