from lib import *
from sys import stdin


points = {Vector(*ints(line)) for line in lines(stdin.read())}
constellations = 0
constellation = None

while points:
    if constellation is None:
        constellation = [points.pop()]
        at = 0

    target_found = False
    until = len(constellation)

    for point in points:
        for other_point in constellation[at:until]:
            if point.manhattan(other_point) <= 3:
                constellation.append(point)
                break

    for point in constellation[until:]:
        points.remove(point)

    at = until

    if until == len(constellation):
        constellations += 1
        constellation = None


print(constellations)
