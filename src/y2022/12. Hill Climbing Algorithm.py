from collections import deque
from sys import stdin

DATA = stdin.read()


def shortest(start, end, descend=False):
    queue = deque([(start, 0)])
    seen = {start}

    while queue:
        index, steps = queue.popleft()
        elevation = ord(DATA[index] if steps else 'a' if DATA[index] == 'S' else 'z')

        for di in (width, 1, -width, -1):
            p = index + di

            if p not in seen:
                if p < 0 or p + 1 > len(DATA):
                    continue

                target_elevation = ord(DATA[p] if DATA[p] != 'E' else 'z')

                if (elevation - target_elevation if descend else target_elevation - elevation) < 2:
                    if end and p == end or descend and DATA[p] == 'a':
                        return steps + 1

                    seen.add(p)
                    queue.append((p, steps + 1))


width = DATA.index('\n') + 1
start, end = map(DATA.index, "SE")

print(shortest(start, end))
print(shortest(end, None, True))
