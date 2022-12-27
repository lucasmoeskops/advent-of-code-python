import os
from collections import defaultdict
from time import sleep

from math import inf
from sys import argv, stdin

try:
    import xtermcolor
except ModuleNotFoundError:
    xtermcolor = None


DATA = stdin.read()
WIDTH = DATA.index('\n')
HEIGHT = len(DATA) // WIDTH
BASIN_WIDTH, BASIN_HEIGHT = WIDTH - 2, HEIGHT - 2
DIRECTIONS = ((0, -1), (-1, 0), (0, 0), (0, 1), (1, 0))
START = DATA.index('.'), 0
END = divmod(DATA.rindex('.'), WIDTH + 1)[::-1]


def get_map(maps, blizzards, minute, for_animate=False):
    if minute not in maps:
        map_ = defaultdict(list) if for_animate else set()
        for (x, y), direction in blizzards.items():
            if for_animate:
                if direction == '>':
                    map_[((x + minute - 1) % BASIN_WIDTH + 1, y)].append(direction)
                elif direction == '<':
                    map_[((x + BASIN_WIDTH - (minute % BASIN_WIDTH) - 1) % BASIN_WIDTH + 1, y)].append(direction)
                elif direction == '^':
                    map_[(x, (y + BASIN_HEIGHT - (minute % BASIN_HEIGHT) - 1) % BASIN_HEIGHT + 1)].append(direction)
                elif direction == 'v':
                    map_[(x, (y + minute - 1) % BASIN_HEIGHT + 1)].append(direction)
            else:
                if direction == '>':
                    map_.add(((x + minute - 1) % BASIN_WIDTH + 1, y))
                elif direction == '<':
                    map_.add(((x + BASIN_WIDTH - (minute % BASIN_WIDTH) - 1) % BASIN_WIDTH + 1, y))
                elif direction == '^':
                    map_.add((x, (y + BASIN_HEIGHT - (minute % BASIN_HEIGHT) - 1) % BASIN_HEIGHT + 1))
                elif direction == 'v':
                    map_.add((x, (y + minute - 1) % BASIN_HEIGHT + 1))
        maps[minute] = map_
        return map_
    return maps[minute]


def construct_animation_route(routes, end, steps):
    route = [end]
    at = end
    while steps:
        at = routes[(at, steps)]
        steps -= 1
        route.append(at)
    return route[::-1]


def print_map(maps, blizzards, minute, position):
    map_ = get_map(maps, blizzards, minute, True)
    out = ''
    for y in range(HEIGHT):
        for x in range(WIDTH):
            if (x, y) == position:
                out += xtermcolor.colorize('E', rgb=0x00ff00)
            elif (x, y) in map_:
                xs = map_[(x, y)]
                if len(xs) > 1:
                    out += xtermcolor.colorize(len(xs), rgb=0xFFFFFF, bg=0x000080)
                else:
                    out += xtermcolor.colorize(xs[0], rgb=0xFFFFFF, bg=0x000080)
            elif (x, y) == START or (x, y) == END:
                out += ' '
            elif x == 0 or y == 0 or x == WIDTH - 1 or y == HEIGHT - 1:
                out += xtermcolor.colorize('#', rgb=0xff0000)
            else:
                out += ' '
        out += '\n'
    return out


def do(animate='-a' in argv):
    blizzards = {}
    maps = {}
    at = START
    total = 0

    for i, c in enumerate(DATA):
        if c in '<>v^':
            row, col = divmod(i, WIDTH + 1)
            blizzards[(col, row)] = c

    full_route = []

    for to in (END, START, END):
        to_x, to_y = to
        dist = to_x + to_y
        queue = [(at, 0)]
        routes = {}
        best = inf
        back = to_y < at[1]
        directions = DIRECTIONS[::-1] if back else DIRECTIONS
        lookup = set()

        while queue:
            (x, y), steps = queue.pop()

            if steps + (x + y - dist if back else dist - x - y) >= best:
                continue

            next_map = get_map(maps, blizzards, steps + 1)

            for dx, dy in directions:
                nx, ny = x + dx, y + dy

                if (nx, ny) == to:
                    if best > steps + 1:
                        routes[((nx, ny), steps + 1)] = x, y
                        best = steps + 1

                    continue

                if (
                    (nx < 1 or nx > WIDTH - 2 or ny < 1 or ny > HEIGHT - 2)
                    and (dx != 0 or dy != 0)
                    or (nx, ny, steps + 1) in lookup
                ):
                    continue

                if (nx, ny) not in next_map and steps + (nx + ny - dist if back else dist - x - y) < best:
                    lookup.add((nx, ny, steps + 1))
                    queue.append(((nx, ny), steps + 1))

                    if animate:
                        routes[((nx, ny), steps + 1)] = x, y

        at = to
        total += best

        if to == END:
            print(total)

        if animate:
            full_route.extend(construct_animation_route(routes, to, best))

    if animate:
        frames = []
        maps = {}
        for i, at in enumerate(full_route):
            print(f'{i * 100 // len(full_route)}%', end='\r', flush=True)
            frames.append(print_map(maps, blizzards, i, at))

        for frame in frames:
            os.system('cls' if os.name == 'nt' else 'clear')
            print(frame)
            sleep(.25)

do()
