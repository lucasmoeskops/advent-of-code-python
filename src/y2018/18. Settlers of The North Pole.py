from lib import *
from sys import stdin


def transform(state):
    new_state = defaultdict(lambda: '.')
    for y in range(50):
        for x in range(50):
            t = state[x, y]
            c = Counter(map(state.get, neighbors2d8(x, y)))
            if t == '.' and c['|'] >= 3:
                new_state[x, y] = '|'
            elif t == '|' and c['#'] >= 3:
                new_state[x, y] = '#'
            elif t == '#' and (c['#'] == 0 or c['|'] == 0):
                new_state[x, y] = '.'
            else:
                new_state[x, y] = t
    return new_state


def key(state):
    return ''.join(state.values())


ip = stdin.read()
field = grid2d(ip)
c = Counter(nth(transform_stream(transform, field), 10).values())
print(c['|'] * c['#'])
c = Counter(item_of_periodic(transform_stream(transform, field), 1000000000, key=key).values())
print(c['|'] * c['#'])
