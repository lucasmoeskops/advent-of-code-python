from collections import defaultdict
from sys import stdin

square = int(stdin.read())
steps = square
r = 1
while steps - r ** 2 > 0:
    r += 2
ring = (r+1) // 2
ring_size = r**2 - (r-2)**2
ring_start = (r-2)**2 if r > 1 else 0
side_length = max(1, ring_size // 4)
side, position = divmod((square - ring_start), side_length)
print(abs(position-side_length//2)+ring-1)

lookup_value = defaultdict(int)
lookup_value[(0, 0)] = 1
value = 1
number = 1
x = 0
y = 0
ring = 1
numbers_in_ring = 1
while value < square:
    number += 1
    numbers_in_ring -= 1
    if not numbers_in_ring:
        ring += 1
        numbers_in_ring = (ring*2-1)**2-(ring*2-3)**2
        x = ring - 1
        y = -ring + 2
        side = 0
    else:
        if side == 0:
            y += 1
            if y == ring - 1:
                side = 1
        elif side == 1:
            x -= 1
            if x == -ring + 1:
                side = 2
        elif side == 2:
            y -= 1
            if y == -ring + 1:
                side = 3
        elif side == 3:
            x += 1
    value = 0
    for dx in range(-1, 2):
        for dy in range(-1, 2):
            value += lookup_value[(x+dx, y+dy)]
    lookup_value[(x, y)] = value

print(value)
