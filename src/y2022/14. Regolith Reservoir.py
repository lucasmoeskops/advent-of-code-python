from collections import defaultdict
from sys import stdin

DATA = stdin.read().split('\n')

map_ = defaultdict(lambda: '.')

# Read map
for line in DATA:
    points = line.split(' -> ')
    x, y = map(int, points[0].split(','))

    for point in points[1:]:
        dx, dy = map(int, point.split(','))
        x_direction = 1 if dx > x else -1
        y_direction = 1 if dy > y else -1

        for rx in range(x, dx + x_direction, x_direction):
            for ry in range(y, dy + y_direction, y_direction):
                map_[(rx, ry)] = '#'

        x, y = dx, dy

max_y = max(y for x, y in map_)
count = 0

# Loop first for infinite bottom then for fixed bottom
for before_bottom in (max_y + 3, max_y + 1):
    at_rest = False

    while not at_rest:
        sx, sy = 500, 0

        while True:
            if sy > max_y + 2:  # Check for infinity
                at_rest = True
                break
            if sy < before_bottom and map_[(sx, sy + 1)] == '.':
                sy += 1
            elif sy < before_bottom and map_[(sx - 1, sy + 1)] == '.':
                sx -= 1
                sy += 1
            elif sy < before_bottom and map_[(sx + 1, sy + 1)] == '.':
                sx += 1
                sy += 1
            else:
                map_[(sx, sy)] = 'o'
                at_rest = sy == 0
                break

    print(sum(1 for t in map_.values() if t == 'o'))
