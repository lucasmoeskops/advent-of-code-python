"""
AoC Day 8 - Space Image Format - in Python.
"""

__author__ = "Lucas Moeskops"
__date__ = "2021-01-17"

from sys import stdin

WIDTH, HEIGHT = 25, 6

chunks = lambda l, xs: [xs[:l]] + (chunks(l, xs[l:]) if len(xs) > l else [])
zero_count = lambda s: s.count('0')
one_two_prod = lambda s: s.count('1') * s.count('2')

layers = chunks(WIDTH * HEIGHT, stdin.read())
fewest_zero_digits = min(layers, key=zero_count)

print(f'1: {one_two_prod(fewest_zero_digits)}')

search = lambda ls, i: ls[0][i] if ls[0][i] in ('0', '1') else search(ls[1:], i)

image = [search(layers, i) for i in range(WIDTH * HEIGHT)]
output = '\n'.join(map(''.join, chunks(WIDTH, image))).replace('0', ' ')
print(f'2:\n{output}')
