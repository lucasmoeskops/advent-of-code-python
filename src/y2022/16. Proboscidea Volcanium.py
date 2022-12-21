from collections import defaultdict, deque
from sys import stdin

DATA = stdin.read().split('\n')

map_ = defaultdict(list)
pressures = defaultdict(int)

for line in DATA:
    _, identifier, _, _, rate, _, _, _, _, *options = line.replace(';', '').replace(',', '').split(' ')
    map_[identifier] = options
    pressures[identifier] = int(rate.split('=')[1])

queue = deque([('AA', 0, 0, 0, ())])
best_at = defaultdict(lambda: -1)

while queue:
    at, minute, rate, pressure, open = queue.popleft()

    if best_at[(at, at in open)] >= pressure:
        continue

    best_at[(at, at in open)] = pressure

    if minute < 30:
        if at not in open:
            queue.append((at, minute+1, rate + pressures[at], pressure + rate, open + (at,)))

        for option in map_[at]:
            queue.append((option, minute+1, rate, pressure + rate, open))

print(max(best_at.values()))

queue = deque([('AAAA', 0, 0, 0, ())])
best_at = defaultdict(lambda: -1)

while queue:
    pos, minute, rate, pressure, open = queue.popleft()

    key = (pos, pos[:2] in open, pos[2:] in open)

    if best_at[key] >= pressure:
        continue

    best_at[key] = pressure

    if minute < 26:
        you_at = pos[:2]
        elephant_at = pos[2:]
        you_open = you_at not in open and pressures[you_at]
        elephant_open = elephant_at != you_at and elephant_at not in open and pressures[elephant_at]
        # You and elephant both open valve
        if you_open and elephant_open:
            queue.append((
                min(you_at, elephant_at) + max(you_at, elephant_at),
                minute+1,
                rate + pressures[you_at] + pressures[elephant_at],
                pressure + rate,
                open + (you_at, elephant_at)
            ))

        # You open valve and elephant moves
        if you_open:
            for option in map_[elephant_at]:
                queue.append((
                    min(you_at, option) + max(you_at, option),
                    minute + 1,
                    rate + pressures[you_at],
                    pressure + rate,
                    open + (you_at,)
                ))

        # You move and elephant opens valve
        if elephant_open:
            for option in map_[you_at]:
                queue.append((
                    min(option, elephant_at) + max(option, elephant_at),
                    minute + 1,
                    rate + pressures[elephant_at],
                    pressure + rate,
                    open + (elephant_at,)
                ))

        # Both move
        # Not fully sure if it is valid to say "Never move if you can open a valve with pressure"
        # if not you_open and not elephant_open:
        for option_you in map_[you_at]:
            for option_elephant in map_[elephant_at]:
                queue.append((min(option_you, option_elephant) + max(option_you, option_elephant), minute + 1, rate, pressure + rate, open))

print(max(best_at.values()))
