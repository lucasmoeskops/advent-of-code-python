from collections import deque
from sys import stdin


DESCRIPTIONS = stdin.read().split('\n')

network = {description.split(' ')[0]: {*description.split(' ', maxsplit=2)[2].split(', ')} for description in DESCRIPTIONS}
queue = deque(['0'])
current_group = {'0'}
in_first_group = None
groups_count = 0

while network:
    while queue:
        name = queue.popleft()
        related = network[name]
        del network[name]
        for program in related:
            if program not in current_group:
                current_group.add(program)
                queue.append(program)

    if not in_first_group:
        in_first_group = len(current_group)
    groups_count += 1

    if not network:
        break

    current_group = {next(iter(network.keys()))}
    queue = deque(current_group)

print(in_first_group)
print(groups_count)
