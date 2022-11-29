from string import ascii_uppercase
from sys import stdin


DIAGRAM = stdin.read().split('\n')

x, y = DIAGRAM[0].index('|'), 0
w, h = len(DIAGRAM[0]), len(DIAGRAM)
direction = 2
letters = ""
num_steps = 0

while 0 <= x < w and 0 <= y < h and DIAGRAM[y][x] != ' ':
    if DIAGRAM[y][x] in ascii_uppercase:
        letters += DIAGRAM[y][x]

    match direction:
        case 0 | 2:
            if DIAGRAM[y][x] == '+':
                if x + 1 < w and DIAGRAM[y][x+1] not in '| ':
                    direction = 1
                else:
                    direction = 3
        case 1 | 3:
            if DIAGRAM[y][x] == '+':
                if y + 1 < h and DIAGRAM[y+1][x] not in '- ':
                    direction = 2
                else:
                    direction = 0

    match direction:
        case 0:
            y -= 1
        case 1:
            x += 1
        case 2:
            y += 1
        case 3:
            x -= 1

    num_steps += 1

print(letters)
print(num_steps)