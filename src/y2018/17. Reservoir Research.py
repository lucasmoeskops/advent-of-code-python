from lib import *
from sys import setrecursionlimit, stdin


CONTINUOUS = 'CONTINUOUS'
FIXED = 'FIXED'
SEEN = 'SEEN'


def eval_node(max_y, x, y):
    if y > max_y:
        return CONTINUOUS

    if grid[x, y] in '#~':
        return FIXED

    if grid[x, y] in '|':
        return SEEN

    continuation = eval_node(max_y, x, y + 1)
    grid[x, y] = '|'

    if continuation == FIXED:
        continuation_left = eval_node(max_y, x - 1, y)
        continuation_right = eval_node(max_y, x + 1, y)

        if continuation_left == FIXED and continuation_right == FIXED:
            i = -1

            while grid[x + i, y] == '|':
                grid[x+i, y] = '~'
                i -= 1

            i = 1

            while grid[x + i, y] == '|':
                grid[x+i, y] = '~'
                i += 1

            grid[x, y] = '~'

        if (continuation_left == FIXED or continuation_right == FIXED) and not (continuation_left == CONTINUOUS or continuation_right == CONTINUOUS):
            return FIXED

    return CONTINUOUS


grid = defaultdict(lambda: ' ')
rules = [[line[0], *ints(line)] for line in stdin.read().split('\n')]

for t, at, s, e in rules:
    if t[0] == 'x':
        for i in range(s, e+1):
            grid[at, i] = '#'
    else:
        for i in range(s, e+1):
            grid[i, at] = '#'

setrecursionlimit(100000)
eval_node(max(y for x, y in grid), 500, min(y for x, y in grid))
c = Counter(grid.values())
print(c['~'] + c['|'])
print(c['~'])
