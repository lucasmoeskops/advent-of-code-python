from enum import Enum
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

    return (
        tx + (hx > tx) - (hx < tx),
        ty + (hy > ty) - (hy < ty),
    )


positions = [(0, 0)] * 10
visited2 = {positions[1]}
visited10 = {positions[-1]}
x, y = positions[0]

for line in DATA.split('\n'):
    direction, amount = line.split(' ')
    dx, dy = Direction[direction].value

    for _ in range(int(amount)):
        positions[0] = x, y = x + dx, y + dy
        for j in range(1, len(positions)):
            positions[j] = update_tail(positions[j-1], positions[j])
        visited2.add(positions[1])
        visited10.add(positions[-1])

print('\n'.join(map(str, map(len, (visited2, visited10)))))
