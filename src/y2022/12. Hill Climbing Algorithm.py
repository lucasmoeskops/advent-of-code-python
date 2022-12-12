from collections import deque
from sys import stdin

DATA = stdin.read()


def shortest(for_part_2=False):
    queue = deque([(end if for_part_2 else start, 0)])
    seen = {end if for_part_2 else start}

    while queue:
        index, steps = queue.popleft()
        elevation = ord(DATA[index] if steps else 'z' if for_part_2 else 'a')

        for di in (width, 1, -width, -1):
            p = index + di

            if p not in seen:
                if p < 0 or p + 1 > len(DATA):
                    continue

                target_elevation = ord(DATA[p] if DATA[p] != 'E' else 'z')

                if (elevation - target_elevation if for_part_2 else target_elevation - elevation) < 2:
                    if for_part_2 and DATA[p] == 'a' or DATA[p] == 'E':
                        return str(steps + 1)

                    seen.add(p)
                    queue.append((p, steps + 1))


width = DATA.index('\n') + 1
start, end = map(DATA.index, "SE")
print('\n'.join(map(shortest, [False, True])))
