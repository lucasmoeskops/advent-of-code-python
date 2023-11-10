"""
AoC Day 22 - Slam Shuffle
"""

__author__ = "Lucas Moeskops"
__date__ = "2023-11-09"
__summary__ = "Play with some cards and see Halley's comet"

from lib import *
from sys import stdin


def get_lcf(num_cards, num_shuffles):
    a, b = 1, 0

    for rule in rules:
        if rule.startswith('deal into'):
            a *= -1
            b *= -1
            b -= 1
        elif rule.startswith('cut'):
            n = ints(rule)[0]
            b = (b - n) % num_cards
        elif rule.startswith('deal with'):
            n = ints(rule)[0]
            a = (a * n) % num_cards
            b = (b * n) % num_cards

    # Thanks, https://codeforces.com/blog/entry/72593#comments
    c, d = 1, 0

    while num_shuffles > 0:
        if num_shuffles % 2:
            c, d = c * a % num_cards, (b * c + d) % num_cards
        num_shuffles //= 2
        a, b = (a * a) % num_cards, (b * a + b) % num_cards

    return c, d


puzzle = stdin.read()
rules = puzzle.split('\n')
a, b = get_lcf(10007, 1)
print((a * 2019 + b) % 10007)

a, b = get_lcf(119315717514047, 101741582076661)
inv = pow(a, -1, 119315717514047)
print(((2020 - b) * inv) % 119315717514047)
