#!/usr/bin/env python3

"""
AoC Day 16 - Reindeer Maze - in Python.
"""

__author__ = "Lucas Moeskops"
__date__ = "2024-12-16"
__summary__ = "Back to year 2015, day 14"

from lib import *


class Node(NamedTuple):
    x: int
    y: int
    prev_node: Optional[int]


puzzle = read_input(2024, 16)
width, height = puzzle.index('\n'), puzzle.count('\n') + 1
sx, sy = [(x, y) for y, row in enumerate(puzzle.split('\n')) for x, v in enumerate(row) if v == 'S'][0]
puzzle = puzzle.replace('\n', '')
puzzle = list(map(list, batched(puzzle, width)))
node_lookup = [Node(sx, sy, None)]
queue = [(0, sx, sy, 1, 0, 0)]
seen = defaultdict(lambda: inf)
max_steps = 0
best_places = set()

while queue:
    steps, x, y, dx, dy, n = heappop(queue)

    if puzzle[y][x] == 'E':
        max_steps = steps
        break

    nx, ny = x + dx, y + dy

    if 0 <= nx < width and 0 <= ny < height and puzzle[ny][nx] != '#' and seen[nx, ny, dx, dy] >= steps + 1:
        seen[nx, ny, dx, dy] = steps + 1
        node_lookup.append(Node(nx, ny, n))
        heappush(queue, (steps + 1, nx, ny, dx, dy, len(node_lookup) - 1))

    for ndx, ndy in ((-dy, dx), (dy, -dx)):
        if seen[x, y, ndx, ndy] >= steps + 1000:
            seen[x, y, ndx, ndy] = steps + 1000
            node_lookup.append(Node(x, y, n))
            heappush(queue, (steps + 1000, x, y, ndx, ndy, len(node_lookup) - 1))

print(steps)

queue.append((steps, x, y, dx, dy, n))
for steps, ox, oy, _, _, n in queue:
    if steps > max_steps or ox != x or oy != y:
        continue
    n = node_lookup[n].prev_node
    best_places.add((x, y))
    while n:
        node = node_lookup[n]
        best_places.add((node.x, node.y))
        n = node_lookup[n].prev_node
print(len(best_places))
