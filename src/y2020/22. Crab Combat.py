#!/usr/bin/env python3

"""
AoC Day 22 - Crab Combat - in Python.

Space cards revisited.
"""

__author__ = "Lucas Moeskops"
__date__ = "2020-12-22"

from collections import deque
from sys import stdin

def round(p1, p2):
    x, y = p1.popleft(), p2.popleft()
    p1.extend([x, y]) if x > y else p2.extend([y, x])

def recursive_round(p1, p2):
    x, y = p1.popleft(), p2.popleft()
    if len(p1) >= x and len(p2) >= y:
        d, e = [p1[f] for f in range(x)], [p2[f] for f in range(y)]
        w = game(d, e, recursive=True)[0]
    else:
        w = 1 if x > y else 2
    p1.extend([x, y]) if w == 1 else p2.extend([y, x])

def game(deck1, deck2, recursive=False):
    d, e, seen = deque(deck1), deque(deck2), set()
    f = recursive_round if recursive else round
    while d and e:
        if recursive:
            state = (*d, -1, *e)
            if state in seen:
                break
            seen.add(state)
        f(d, e)
    w = d or e
    return (1 if d else 2), sum([x * (len(w) - i) for i, x in enumerate(w)])

player1, player2 = stdin.read().split('\n\n')
deck1, deck2 = [[int(x) for x in p.split('\n')[1:]] for p in [player1, player2]]

print(f'1: {game(deck1, deck2)[1]}')
print(f'2: {game(deck1, deck2, recursive=True)[1]}')