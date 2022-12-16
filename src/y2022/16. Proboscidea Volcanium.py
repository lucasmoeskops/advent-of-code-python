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

queue = deque([('AA', 'AA', 0, 0, 0, ())])
best_at = defaultdict(lambda: -1)

while queue:
    you_at, elephant_at, minute, rate, pressure, open = queue.popleft()

    if best_at[(you_at, you_at in open, elephant_at, elephant_at in open)] >= pressure:
        continue

    best_at[(you_at, you_at in open, elephant_at, elephant_at in open)] = pressure

    if minute < 26:
        # You and elephant both open valve
        if you_at not in open and elephant_at not in open and pressures[you_at] and pressures[elephant_at]:
            queue.append((
                you_at,
                elephant_at,
                minute+1,
                rate + pressures[you_at] + pressures[elephant_at],
                pressure + rate,
                open + (you_at, elephant_at)
            ))

        # You open valve and elephant moves
        if you_at not in open and pressures[you_at]:
            for option in map_[elephant_at]:
                queue.append((
                    you_at,
                    option,
                    minute + 1,
                    rate + pressures[you_at],
                    pressure + rate,
                    open + (you_at,)
                ))

        # You move and elephant opens valve
        if elephant_at not in open and pressures[elephant_at]:
            for option in map_[you_at]:
                queue.append((
                    option,
                    elephant_at,
                    minute + 1,
                    rate + pressures[elephant_at],
                    pressure + rate,
                    open + (elephant_at,)
                ))

        # Both move
        for option_you in map_[you_at]:
            for option_elephant in map_[elephant_at]:
                queue.append((option_you, option_elephant, minute + 1, rate, pressure + rate, open))

print(max(best_at.values()))
