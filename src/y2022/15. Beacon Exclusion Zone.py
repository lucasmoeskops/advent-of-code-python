from re import findall
from sys import stdin

DATA = stdin.read().strip().split('\n')
SPACE = 4000000
LOOKING_FOR = 2000000


def manhattan(x, y, x2, y2):
    return abs(x2 - x) + abs(y2 - y)


sensors = [tuple(map(int, findall(r'(-?\d+)', line))) for line in DATA]
sensors = [(x, y, manhattan(x, y, closest_x, closest_y)) for x, y, closest_x, closest_y in sensors]
sensors = sorted(sensors, key=lambda sensor: sensor[0])
cover_min, cover_max = 0, 0

for x, y, distance in sensors:
    x_size = distance - abs(LOOKING_FOR - y)

    if x - x_size < cover_min:
        cover_min = x - x_size

    if x + x_size > cover_max:
        cover_max = x + x_size

print(cover_max - cover_min)

for y in range(SPACE):
    x = 0

    for sensor_x, sensor_y, size in sensors:
        x_size = size - abs(y - sensor_y)

        if abs(sensor_x - x) <= x_size:
            x = sensor_x + x_size + 1

    if x <= SPACE:
        print(x * SPACE + y)
        break


# Smart solution by 5space: increase size by 1 and check intersection points
# (since only 1 solution it must be there)
# for sensor_x, sensor_y, size in sensors:
#     for sensor2_x, sensor2_y, size2 in sensors:
