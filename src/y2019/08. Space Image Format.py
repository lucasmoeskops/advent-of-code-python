"""
AoC Day 8 - Space Image Format - in Python.
"""

__author__ = "Lucas Moeskops"
__date__ = "2021-01-17"
__summary__ = "Decode picture sent by elf for reboot password of Mars Rover"

from sys import stdin

WIDTH, HEIGHT = 25, 6


def chunks(l, xs):
    return [xs[:l]] + (chunks(l, xs[l:]) if len(xs) > l else [])


def zero_count(s):
    return s.count('0')


def one_two_prod(s):
    return s.count('1') * s.count('2')


def search(ls, i):
    return ls[0][i] if ls[0][i] in ('0', '1') else search(ls[1:], i)


layers = chunks(WIDTH * HEIGHT, stdin.read())
fewest_zero_digits = min(layers, key=zero_count)

print(one_two_prod(fewest_zero_digits))

image = [search(layers, i) for i in range(WIDTH * HEIGHT)]
output = '\n'.join(map(''.join, chunks(WIDTH, image))).replace('0', '.').replace('1', '#')
print(output)
