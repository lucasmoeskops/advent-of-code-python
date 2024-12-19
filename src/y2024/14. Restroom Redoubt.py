#!/usr/bin/env python3

"""
AoC Day 14 - Restroom Redoubt - in Python.
"""

__author__ = "Lucas Moeskops"
__date__ = "2024-12-14"
__summary__ = "Back to year 2016, day 2"


from lib import batched, prod, read_input, re


puzzle = read_input(2024, 14)
ints = map(int, re.findall(r'-?\d+', puzzle))
robots = list(map(list, batched(ints, 4)))
width = 101
height = 103
best, best_at = 0, 0

for second in range(width * height):
    filled = set()
    correlation_score = 0

    if second == 100:
        per_quadrant = [0, 0, 0, 0]
        for robot in robots:
            if robot[0] == width // 2 or robot[1] == height // 2:
                continue
            quadrant = 2 * (robot[0] >= width // 2) + (robot[1] >= height // 2)
            per_quadrant[quadrant] += 1
        print(prod(per_quadrant))

    for robot in robots:
        robot[0] = (robot[0] + robot[2]) % width
        robot[1] = (robot[1] + robot[3]) % height
        filled.add((robot[0], robot[1]))

    for x, y in filled:
        correlation_score += ((x-1, y) in filled)# + ((x, y-1) in filled)

    if correlation_score > best:
        best = correlation_score
        best_at = second

print(best_at)
