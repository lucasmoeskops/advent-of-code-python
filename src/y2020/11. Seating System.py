from sys import stdin

def adjacent(w, h, x, y, state):
    fields = ((x+dx, y+dy) for (dx, dy) in directions)
    return ((x, y) for (x, y) in fields if 0 <= x < w and 0 <= y < h)

def seeable(w, h, x, y, state):
    seen = []
    for dx, dy in directions:
        p, q = x, y
        while True:
            p += dx
            q += dy
            if not (0 <= p < w and 0 <= q < h):
                break
            if state[q][p] in ('L', '#'):
                seen.append((p, q))
                break
    return seen

def round(determinator, tolerancy, state):
    changes = []
    h, w = len(state), len(state[0])
    for y in range(h):
        for x in range(w):
            char = state[y][x]
            if char == '.':
                continue
            xs = [state[y][x] for (x, y) in determinator(w, h, x, y, state)]
            if char == 'L' and all(x != '#' for x in xs):
                changes.append((x, y, '#'))
            elif char == '#' and sum(1 for x in xs if x == '#') >= tolerancy:
                changes.append((x, y, 'L'))
    for x, y, c in changes:
        state[y][x] = c
    return bool(changes)

def simulate(determinator, tolerancy, state):
    while round(determinator, tolerancy, state):
        pass
    return sum(1 for row in state for char in row if char == '#')

new_state = lambda: [list(line) for line in lines]

directions = [(dx, dy) for dy in range(-1, 2) for dx in range(-1, 2) if dy or dx]

lines = stdin.read().split('\n')

print(f'1: {simulate(adjacent, 4, new_state())}')
print(f'2: {simulate(seeable, 5, new_state())}')