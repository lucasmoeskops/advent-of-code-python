"""
AoC Day 4 - Secure Container - in Python.
"""

__author__ = "Lucas Moeskops"
__date__ = "2023-11-4"
__summary__ = "Recover password to use Venus fuel depot"

from lib import *
from sys import stdin


def is_valid(n):
    s = str(n)

    return any(a == b for a, b in pairwise(s)) and all(a > b for a, b in pairwise(s))


start, end = ints(stdin.read())
n = start
matching = 0

while n <= end:
    s = str(n)

    for i, (a, b) in enumerate(pairwise(s)):
        if a > b:
            n = int(s[:i+1] + s[i] * (len(s) - i - 1))
            break

    if n > end:
        break

    for a, b in pairwise(str(n)):
        if a == b:
            matching += 1
            break

    n += 1

print(matching)
n = start
matching = 0

while n <= end:
    s = str(n)

    for i, (a, b) in enumerate(pairwise(s)):
        if a > b:
            n = int(s[:i+1] + s[i] * (len(s) - i - 1))
            break

    if n > end:
        break

    s = str(n)

    for i, (a, b) in enumerate(pairwise(s)):
        if a == b and (i == 0 or s[i-1] != a) and (i == 4 or s[i+2] != b):
            matching += 1
            break

    n += 1

print(matching)
