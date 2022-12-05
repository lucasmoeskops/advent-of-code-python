from itertools import takewhile
from operator import itemgetter
from sys import stdin


SETUP, MOVEMENT = stdin.read().split('\n\n')
MOVEMENT_GETTER = itemgetter(1, 3, 5)


def parse_movement(movement):
    return tuple(map(int, MOVEMENT_GETTER(movement.split(' '))))


def parse_stacks():
    return [list(takewhile(' '.__ne__, col)) for col in zip(*(line[1::4] for line in SETUP.split('\n')[-2::-1]))]


movements = list(map(parse_movement, MOVEMENT.split('\n')))

stacks = parse_stacks()

for amount, take_from, place in movements:
    stacks[place - 1].extend(stacks[take_from - 1][-amount:][::-1])
    stacks[take_from - 1] = stacks[take_from - 1][:-amount]

print(''.join(stack[-1] for stack in stacks))

stacks = parse_stacks()

for amount, take_from, place in movements:
    stacks[place-1].extend(stacks[take_from-1][-amount:])
    stacks[take_from-1] = stacks[take_from-1][:-amount]

print(''.join(stack[-1] for stack in stacks))
