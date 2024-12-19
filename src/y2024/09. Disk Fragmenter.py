#!/usr/bin/env python3

"""
AoC Day 9 - Disk Fragmenter - in Python.
"""

__author__ = "Lucas Moeskops"
__date__ = "2024-12-09"
__summary__ = "Back to year 2021, day 23"

from lib import itemgetter, read_input


puzzle = read_input(2024, 9)
areas = list(map(int, puzzle))
areas_2 = list(areas)
start, end = 0, len(areas) - 1
total = 0
index = 0

while start < end:
    if start % 2:
        m = min(areas[start], areas[end])

        for i in range(m):
            total += (end // 2) * index
            index += 1

        areas[start] -= m
        areas[end] -= m

        if not areas[start]:
            start += 1

        if not areas[end]:
            end -= 2

    else:
        for i in range(areas[start]):
            total += (start // 2) * index
            index += 1

        start += 1

print(total)

placed = []
biggest = 9
starts = [1] * 10

for i in range(len(areas_2)-1, -1, -2):
    size = areas_2[i]

    if not (0 < size <= biggest):
        continue

    for j in range(starts[size], i, 2):
        if areas_2[j] >= areas_2[i]:
            areas_2[j] -= areas_2[i]
            placed.append((j, areas_2[i], i // 2))
            areas_2[i] *= -1
            starts[size] = j
            break
    else:
        biggest = areas_2[i] - 1

placed.sort(key=itemgetter(0))
placed = placed[::-1]
index = 0
total = 0

for i in range(len(areas_2)):
    if i % 2:
        if placed:
            while placed and placed[-1][0] == i:
                _, times, amount = placed.pop()
                for j in range(times):
                    total += amount * index
                    index += 1

        index += areas_2[i]

    else:
        if areas_2[i] < 0:
            index -= areas_2[i]
        else:
            for j in range(areas_2[i]):
                total += (i // 2) * index
                index += 1

print(total)
