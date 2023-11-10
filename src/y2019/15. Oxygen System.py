"""
AoC Day 15 - Oxygen System.
"""

__author__ = "Lucas Moeskops"
__date__ = "2023-11-06"
__summary__ = "Repair an oxygen system by controlling a drone"

from lib import *
from sys import stdin
from y2019.computer import make_runner


DIRECTION_MAP = (1, 4, 2, 3)


def get_options(grid, x, y):
    for nx, ny in neighbors2d4(x, y):
        if grid[nx, ny] in '+X':
            yield nx, ny


def evaluate(ex, ey, x, y):
    return ex == x and ey == y


def move(grid, send, sx, sy, ex, ey):
    routes = bfs([(sx, sy)], curry(partial(get_options, grid)), curry(partial(evaluate, ex, ey)))
    route = routes[0] + [(ex, ey)]
    x, y = sx, sy

    for nx, ny in route:
        if nx > x:
            send(DIRECTION_MAP[1])
        elif nx < x:
            send(DIRECTION_MAP[3])
        elif ny > y:
            send(DIRECTION_MAP[2])
        elif ny < y:
            send(DIRECTION_MAP[0])

        x, y = nx, ny

    return True


def explore(grid, send, set_target, x, y):
    options = []

    for d in range(4):
        p = move2d(x, y, d)

        if grid[p] != '_':
            continue

        message = send(DIRECTION_MAP[d])

        if message > 0:
            grid[p] = '+'
            options.append(p)
            assert send(DIRECTION_MAP[(d+2) % 4]) > 0
        else:
            grid[p] = '#'

        if message == 2:
            grid[p] = '+'
            set_target(*move2d(x, y, d))

    return options


def run():
    def send_message(x):
        in_.append(x)
        next(runner)
        return out.popleft()

    def set_target(x, y):
        nonlocal target
        target = (x, y)

    grid = grid2d('', default='_')
    unexplored = [(0, 0)]
    in_, out, target = deque(), deque(), (inf, inf)
    runner = make_runner(program, in_, out)
    x, y = 0, 0
    while unexplored:
        nx, ny = unexplored.pop()
        move(grid, send_message, x, y, nx, ny)
        unexplored.extend(explore(grid, send_message, set_target, nx, ny))
        x, y = nx, ny
    return grid, target
    # printgrid(grid, size=None, default=' ')
    solutions = bfs([(0, 0)], curry(partial(get_options, grid)), curry(partial(evaluate, *target)))
    return len(solutions[0])
    # print(solutions)


program = [int(code) for code in stdin.read().split(',')]
grid, target = run()
distance_table = distances([target], curry(partial(get_options, grid)))
print(distance_table[0, 0])
print(max(distance_table.values()))
