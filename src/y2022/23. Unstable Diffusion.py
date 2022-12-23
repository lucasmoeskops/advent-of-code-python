from collections import defaultdict
from itertools import cycle, islice, count
from math import prod
from sys import stdin

DATA = stdin.read()

N = 0
NE = 1
E = 2
SE = 3
S = 4
SW = 5
W = 6
NW = 7

DIRECTIONS = {
    N: (0, -1),
    NE: (1, -1),
    E: (1, 0),
    SE: (1, 1),
    S: (0, 1),
    SW: (-1, 1),
    W: (-1, 0),
    NW: (-1, -1),
}

CONSIDER_ORDER = (N, S, W, E)


def make_cycle(items, start):
    return islice(cycle(items), start, start + len(items))


def round(positions, considering_first):
    considered_moves = defaultdict(list)
    has_move = set()

    for x, y in positions:
        for dx, dy in DIRECTIONS.values():
            if (x + dx, y + dy) in positions:
                break
        else:
            has_move.add((x, y))

    if len(has_move) == len(positions):
        return True

    for direction in make_cycle(CONSIDER_ORDER, considering_first):
        d = DIRECTIONS[(direction - 1) % 8]
        e = DIRECTIONS[direction]
        f = DIRECTIONS[(direction + 1) % 8]

        for x, y in positions:
            if (x, y) in has_move:
                continue

            for dx, dy in (d, e, f):
                if (x + dx, y + dy) in positions:
                    break
            else:
                # can move
                considered_moves[(x + e[0], y + e[1])].append((x, y))
                has_move.add((x, y))

    for location, elves in considered_moves.items():
        if len(elves) == 1:
            positions.remove(elves[0])
            positions.add(location)

    return False


def do():
    considering_first = 0
    positions = {
        (x, y)
        for y, row in enumerate(DATA.split('\n'))
        for x, val in enumerate(row) if val == '#'
    }

    for number in count(start=1):
        if round(positions, considering_first):
            print(number)  # part 2
            break

        considering_first = (considering_first + 1) % 4

        if number == 10:
            print(
                prod(
                    max(vs) - min(vs) + 1
                    for vs in ([p[d] for p in positions] for d in range(2))
                ) - len(positions)
            )  # part 1


do()
