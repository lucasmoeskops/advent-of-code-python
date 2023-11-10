"""
AoC Day 17 - Set and Forget.
"""

__author__ = "Lucas Moeskops"
__date__ = "2023-11-09"
__summary__ = "Save robots from the solar flare"

from lib import *
from sys import stdin
from y2019.computer import make_runner


def run():
    in_, out, target = deque(), deque(), (inf, inf)
    runner = make_runner(program, in_, out)
    next(runner)
    grid = grid2d(''.join(map(chr, out)), default='_')
    alignment_sum = 0

    for x, y in grid:
        if grid[x, y] == '#' and all(n in grid and grid[n] == '#' for n in neighbors2d4(x, y)):
            alignment_sum += x * y

    x, y = next(t for t, c in grid.items() if c in '^v<>')
    route = []
    last_direction = '^>v<'.index(grid[x, y])

    while True:
        directions = [d for d, n in enumerate(neighbors2d4(x, y)) if d % 2 != last_direction % 2 and grid[n] == '#']
        if not directions:
            break
        assert len(directions) == 1, f"multiple options at {x},{y}"
        direction = directions[0]
        length = 0
        while grid[move2d(x, y, direction)] == '#':
            length += 1
            x, y = move2d(x, y, direction)
        route.append('R' if direction == (last_direction + 1) % 4 else 'L')
        route.append(str(length))
        last_direction = direction

    return alignment_sum, ''.join(route)


def run2():
    in_, out, target = deque(), deque(), (inf, inf)
    program = [int(code) for code in puzzle.split(',')]
    program[0] = 2
    in_.extend(chain.from_iterable(list(map(ord, s) for s in (
        'A,B,A,C,B,A,C,B,A,C\n',
        'L,12,L,12,L,6,L,6\n',
        'R,8,R,4,L,12\n',
        'L,12,L,6,R,12,R,8\n',
        'n\n'
    ))))
    runner = make_runner(program, in_, out)
    next(runner)
    return out.pop()


puzzle = stdin.read()
program = [int(code) for code in puzzle.split(',')]
alignment_sum, route = run()
print(alignment_sum)
dust_cleaned = run2()
print(dust_cleaned)

