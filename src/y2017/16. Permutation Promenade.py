from collections import deque
from string import ascii_lowercase
from sys import stdin


MOVES = stdin.read().split(',')


def dance(rounds):
    programs = deque(ascii_lowercase[:16])
    seen = {}

    for round in range(rounds):
        for move in MOVES:
            if move[0] == 's':
                programs.rotate(int(move[1:]))
            elif move[0] == 'x':
                a, b = map(int, move[1:].split('/'))
                t = programs[a]
                programs[a] = programs[b]
                programs[b] = t
            elif move[0] == 'p':
                a, b = move[1:].split('/')
                t = programs.index(a)
                u = programs.index(b)
                programs[t] = b
                programs[u] = a

        out = ''.join(programs)
        if out in seen:
            return dance((rounds-round) % (round - seen[out]))
        seen[out] = round

    return ''.join(programs)


print(dance(1))
print(dance(1000**3))
