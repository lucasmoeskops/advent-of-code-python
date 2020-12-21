#!/usr/bin/env python3

"""
AoC Day 20 - Jurassic Jigsaw - in Python.

Troubled waters.
"""

__author__ = "Lucas Moeskops"
__date__ = "2020-12-21"

from collections import defaultdict, deque
from math import ceil, prod, sqrt
from operator import itemgetter
from sys import stdin

sea_monster = [
'                  # ',
'#    ##    ##    ###',
' #  #  #  #  #  #   ']

neighbours = [(0, -1), (1, 0), (0, 1), (-1, 0)]

def parse_tile(tile):
    number_str, data_str = tile.split('\n', 1)
    number = int(number_str[5:-1])
    l = data_str.split('\n')
    data = {(x, y) for y, r in enumerate(l) for x, c in enumerate(r) if c == '#'}
    top_border = tuple(sorted(x for (x, y) in data if y == 0))
    right_border = tuple(sorted(y for (x, y) in data if x == 9))
    bottom_border = tuple(sorted(x for (x, y) in data if y == 9))
    left_border = tuple(sorted(y for (x, y) in data if x == 0))
    borders = (top_border, right_border, bottom_border, left_border)
    rest = {(x - 1, y - 1) for x, y in data if 0 < x < 9 and 0 < y < 9}
    return (number, borders, rest)

reverse_border = lambda b: tuple(reversed([9 - x for x in b]))
rotate = lambda d, w=7: {(y, w - x) for (x, y) in d}
flip = lambda d, w=7: {(w - x,y) for (x, y) in d}
mask = lambda mbs, bs: [(b if mbs[i] else None) for i, b in enumerate(bs)]

def matching_border(tiles, s, t):
    bs_s = tiles[s][1]
    bs_t = tiles[t][1]
    for border in bs_s:
        if border in bs_t or reverse_border(border) in bs_t:
            return border

def build_border_map(tiles):  # what borders are seen on what tiles
    border_map = defaultdict(list)
    for n, borders, d in tiles.values():
        for border in borders:
            border_map[border].append(n)
            border_map[reverse_border(border)].append(n)
    return border_map

def build_connection_map(border_map):  # what pieces are connected
    connection_map = defaultdict(set)
    for border, matches in list(border_map.items()):
        if len(matches) == 2:
            m, n = matches
            connection_map[m].add(n)
            connection_map[n].add(m)
    return connection_map

def build_map_plan(connection_map, start):  # where do the pieces go
    pt_map = {(0, 0): start}
    tp_map = {start: (0, 0)}
    queue = deque(connection_map[start])
    while queue:
        s = queue.popleft()
        if s in tp_map:
            continue
        around = [tp_map[t] for t in connection_map[s] if t in tp_map]
        if len(around) == 1:
            ax, ay = around[0]
            x, y = (ax + 1, ay) if (ax + 1, ay) not in pt_map else (ax, ay + 1)
        else:
            ax, ay = sum(x for x, y in around), sum(y for x, y in around)
            x, y = ceil(ax / len(around)), ceil(ay / len(around))
        pt_map[(x, y)], tp_map[s] = s, (x, y)
        queue.extend(connection_map[s])
    return pt_map, tp_map

def build_transform_map(tiles, pt_map):  # how are the pieces transformed
    transform_map = {}
    for (x, y), s in pt_map.items():
        bs = tiles[s][1]
        mbs = [(matching_border(tiles, s, pt_map[p]) if p in pt_map else None)
               for p in ((x+dx, y+dy) for (dx, dy) in neighbours)]
        for r in range(4):
            if mbs == mask(mbs, [bs[(r + i) % 4] for i in range(4)]):
                transform_map[s] = False, r
                break
            if mbs == mask(mbs, [bs[(r - i) % 4] for i in range(4)]):
                transform_map[s] = True, r
                break
    return transform_map

def build_sea_map(tiles, tile_position_map, rotate_map):  # what does the sea look like
    sea_map = set()
    for s, (f, r) in rotate_map.items():
        x, y = tile_position_map[s]
        _, _, data = tiles[s]
        for i in range(r):
            data = rotate(data)
        if f:
            data = flip(data)
        sea_map.update((x * 8 + dx, y * 8 + dy) for (dx, dy) in data)
    return sea_map

def determine_roughness(size, sea_map, pattern):  # what is the monster density
    for flips in (0, 1):
        for rotates in (0, 3):
            hits = set()
            for (x, y) in sea_map:
                ps = set()
                for (dx, dy) in pattern:
                    p = (x + dx, y + dy - 1)
                    if p not in sea_map:
                        break
                    ps.add(p)
                else:
                    hits |= ps
            if hits:
                return len(sea_map) - len(hits)
            sea_map = rotate(sea_map, w=size*8)
        sea_map = flip(sea_map, w=size*8)

tiles_raw = stdin.read().split('\n\n')
tiles = [parse_tile(tile) for tile in tiles_raw]
tiles = {x[0]: x for x in tiles}

border_map = build_border_map(tiles)
connection_map = build_connection_map(border_map)
edges = [x for (x, s) in connection_map.items() if len(s) == 2]

print(f'1: {prod(edges)}')

tile_position_map, position_tile_map = build_map_plan(connection_map, edges[0])
transform_map = build_transform_map(tiles, tile_position_map)
sea_map = build_sea_map(tiles, position_tile_map, transform_map)
size = int(sqrt(len(connection_map)))
sea_monster_pattern = {
    (x, y) for y, l in enumerate(sea_monster) for x, c in enumerate(l) if c == '#'}
print(f'2: {determine_roughness(size, sea_map, sea_monster_pattern)}')