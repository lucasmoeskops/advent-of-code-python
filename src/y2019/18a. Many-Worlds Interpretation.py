from collections import deque
from string import ascii_lowercase
from sys import stdin

DATA = stdin.read().strip()


def bfs(vault_map, num_keys):
    starts = [i for i, c in enumerate(vault_map) if c == '@']
    queue = deque([(tuple(starts), 0, tuple(ascii_lowercase[:num_keys]))])
    seen = {(start, tuple(ascii_lowercase[:num_keys])) for start in starts}
    width = vault_map.index('\n') + 1
    highest = 0
    best = 0

    while queue:
        positions, steps, keys = queue.popleft()

        # if len(keys) > best:
        #     best = len(keys)

        if len(keys) == 0:
            return steps

        if steps > highest:
            # print(steps)
            highest = steps

        options = []

        for i, position in enumerate(positions):
            for di in (width, 1, -width, -1):
                p = position + di

                if vault_map[p] == '#':
                    continue

                if 'A' <= vault_map[p] <= 'Z' and vault_map[p].lower() in keys:
                    continue

                if (p, keys) in seen:
                    continue

                options.append((i, p))

        for i, p in options:
            new_key = 'a' <= vault_map[p] <= 'z' and vault_map[p] in keys and vault_map[p]
            new_ps = list(positions)
            new_ps[i] = p

            seen.add((p, keys))
            queue.append((tuple(new_ps), steps + 1, tuple(k for k in keys if k != new_key) if new_key else keys))


def do():
    start = DATA.index('@')
    width = DATA.index('\n') + 1
    num_keys = sum(1 for c in DATA if 'a' <= c <= 'z')

    print(bfs(DATA, num_keys))

    if DATA.count('@') > 1:
        return

    data = list(DATA)
    data[start-width-1] = '@'
    data[start-width] = '#'
    data[start-width+1] = '@'
    data[start-1] = '#'
    data[start] = '#'
    data[start+1] = '#'
    data[start+width-1] = '@'
    data[start+width] = '#'
    data[start+width+1] = '@'

    print(bfs(''.join(data), num_keys))


do()
