from collections import Counter
from sys import stdin

DATA = stdin.read().split('\n')

sides = Counter()
for line in DATA:
    x, y, z = map(int, line.split(','))
    sides[(x, x+1, y, y+1, z, z)] += 1
    sides[(x, x+1, y, y+1, z+1, z+1)] += 1
    sides[(x, x+1, y, y, z, z+1)] += 1
    sides[(x, x+1, y+1, y+1, z, z+1)] += 1
    sides[(x, x, y, y+1, z, z+1)] += 1
    sides[(x+1, x+1, y, y+1, z, z+1)] += 1
for side, num in tuple(sides.items()):
    if num != 1:
        del sides[side]
print(len(sides))

cubes = set()
low_x = high_x = low_y = high_y = low_z = high_z = None
for line in DATA:
    x, y, z = map(int, line.split(','))
    if low_x is None:
        low_x = high_x = x
        low_y = high_y = y
        low_z = high_z = z
    else:
        low_x = min(x, low_x)
        low_y = min(y, low_y)
        low_z = min(z, low_z)
        high_x = max(x, high_x)
        high_y = max(y, high_y)
        high_z = max(z, high_z)
    cubes.add((x, y, z))

escapable = set()
inescapable = set(cubes)


def escape(x, y, z):
    global escapable, inescapable
    queue = [(x, y, z)]
    seen = {(x, y, z)}
    while queue:
        x, y, z = queue.pop()

        if (x, y, z) in escapable:
            escapable |= seen
            return True

        if (x, y, z) in inescapable:
            inescapable |= seen
            return False

        if x <= low_x or x >= high_x or y <= low_y or y >= high_y or z <= low_z or z >= high_z:
            escapable |= seen
            return True

        if (x - 1, y, z) not in seen and (x - 1, y, z) not in cubes:
            seen.add((x - 1, y, z))
            queue.append((x - 1, y, z))

        if (x + 1, y, z) not in seen and (x + 1, y, z) not in cubes:
            seen.add((x + 1, y, z))
            queue.append((x + 1, y, z))

        if (x, y - 1, z) not in seen and (x, y - 1, z) not in cubes:
            seen.add((x, y - 1, z))
            queue.append((x, y - 1, z))

        if (x, y + 1, z) not in seen and (x, y + 1, z) not in cubes:
            seen.add((x, y + 1, z))
            queue.append((x, y + 1, z))

        if (x, y, z - 1) not in seen and (x, y, z - 1) not in cubes:
            seen.add((x, y, z - 1))
            queue.append((x, y, z - 1))

        if (x, y, z + 1) not in seen and (x, y, z + 1) not in cubes:
            seen.add((x, y, z + 1))
            queue.append((x, y, z + 1))

    inescapable |= seen
    return False


directions = {}
for line in DATA:
    x, y, z = map(int, line.split(','))
    directions[(x, x+1, y, y+1, z, z)] = 1
    directions[(x, x+1, y, y+1, z+1, z+1)] = 2
    directions[(x, x+1, y, y, z, z+1)] = 3
    directions[(x, x+1, y+1, y+1, z, z+1)] = 4
    directions[(x, x, y, y+1, z, z+1)] = 5
    directions[(x+1, x+1, y, y+1, z, z+1)] = 6

exterior = 0
for side in sides:
    x, x1, y, y1, z, z1 = side

    if (
            directions[side] == 1 and escape(x, y, z - 1)
            or directions[side] == 2 and escape(x, y, z)
            or directions[side] == 3 and escape(x, y - 1, z)
            or directions[side] == 4 and escape(x, y, z)
            or directions[side] == 5 and escape(x - 1, y, z)
            or directions[side] == 6 and escape(x, y, z)
    ):
        exterior += 1
print(exterior)
