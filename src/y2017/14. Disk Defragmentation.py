from collections import deque, Counter
from functools import reduce
from itertools import count
from operator import itemgetter
from sys import stdin


INPUT = stdin.read()
ADJACENT = [(0, 1), (-1, 0), (0, -1), (1, 0)]


def make_knot_hash(s):
    lengths = [ord(c) for c in s] + [17, 31, 73, 47, 23]
    deque_ = deque(range(256))
    skip_size = 0
    total_skipped = 0
    for _ in range(64):
        for length in lengths:
            deque_.extendleft([deque_.popleft() for _ in range(length)])
            deque_.rotate(-length - skip_size)
            total_skipped += length + skip_size
            skip_size += 1
    deque_.rotate(total_skipped)
    return ''.join(hex(reduce(int.__xor__, (deque_.popleft() for _ in range(16))))[2:].zfill(2) for _ in range(16))


grid = {(x, y) for y in range(128) for x, used in enumerate(bin(int(make_knot_hash(f'{INPUT}-{y}'), 16))[2:].zfill(128)) if used == '1'}
print(len(grid))

num_regions = 0

while grid:
    queue = [next(iter(grid))]
    grid.remove(queue[0])

    while queue:
        x, y = queue.pop()
        for dx, dy in ADJACENT:
            if (x+dx, y+dy) in grid:
                queue.append((x+dx, y+dy))
                grid.remove((x+dx, y+dy))

    num_regions += 1

print(num_regions)
