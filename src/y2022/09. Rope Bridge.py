from enum import Enum
from itertools import pairwise, starmap

from sys import stdin

DATA = stdin.read()


class Direction(Enum):
    U = 0, -1
    R = 1, 0
    D = 0, 1
    L = -1, 0


def update_tail(head, tail):
    hx, hy = head
    tx, ty = tail

    if abs(hx-tx) <= 1 and abs(hy-ty) <= 1:
        return tail

    if hx > tx:
        tx += 1
    elif hx < tx:
        tx -= 1

    if hy > ty:
        ty += 1
    elif hy < ty:
        ty -= 1

    return tx, ty


positions = [(0, 0)] * 10
visited2 = {positions[-1]}
visited10 = {positions[-1]}
x, y = positions[0]

for line in DATA.split('\n'):
    direction, amount = line.split(' ')
    dx, dy = Direction[direction].value

    for i in range(int(amount)):
        positions[1:] = starmap(update_tail, pairwise(positions))
        visited2.add(positions[1])
        visited10.add(positions[-1])
        x += dx
        y += dy
        positions[0] = x, y

print('\n'.join(map(str, map(len, (visited2, visited10)))))
