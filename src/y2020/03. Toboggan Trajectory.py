from sys import stdin
from functools import partial, reduce
from math import prod

def trees(hill, slope):
    x, y = slope
    w = hill.index('\n')
    wl = w + 1
    h = len(hill) // wl + 1
    glide = lambda cnt, sx, sy: (cnt + (hill[sx + sy] == '#'), (sx + x) % w, sy + wl * y)
    return reduce(lambda state, _: glide(*state), range(0, h, y), (0, 0, 0))[0]

slopes = (1, 1), (3, 1), (5, 1), (7, 1), (1, 2)

hill = stdin.read()

print(f'1: {trees(hill, slopes[1])}')
print(f'2: {prod(map(partial(trees, hill), slopes))}')