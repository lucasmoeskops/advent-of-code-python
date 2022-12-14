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
at_rest = False

while not at_rest:
    sx, sy = 500, 0

    while True:
        if sy > max_y:
            at_rest = True
            break
        if map_[(sx, sy + 1)] == '.':
            sy += 1
        elif map_[(sx - 1, sy + 1)] == '.':
            sx -= 1
            sy += 1
        elif map_[(sx + 1, sy + 1)] == '.':
            sx += 1
            sy += 1
        else:
            map_[(sx, sy)] = 'o'
            break

print(sum(1 for t in map_.values() if t == 'o'))

count = 1
bound_right, bound_left = -500, 500

for y in range(1, max_y + 2):
    streak = map_[(-y - 1, y-1)] != '#' + map_[(-y - 1, y-1)] != '#'
    for x in range(-y + bound_left, y + 1 - bound_right):
        streak = max(0, streak + 1 if map_[(x+1, y-1)] != '#' else 3)
        if map_[(x, y)] == '#':
            if map_[(x - 1, y)] == '#' == map_[(x + 1, y)]:
                map_[(x, y + 1)] = '#'
            if x == -y + bound_left:
                bound_left += 1
            elif x == y + 1 - bound_right:
                bound_right += 1
                while map_[(y + 1 - bound_right, y)] == '#':
                    bound_right += 1
        elif streak:
            count += 1

print(count)
