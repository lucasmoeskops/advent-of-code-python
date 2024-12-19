#!/usr/bin/env python3

"""
AoC Day 12 - Garden Groups - in Python.
"""

__author__ = "Lucas Moeskops"
__date__ = "2024-12-12"
__summary__ = "Back to year 2023, day 5 and 21"

from lib import List, Tuple, batched, pairwise, read_input


def find_shape_and_size(x, y, c, container) -> int:
    if puzzle[y][x] != c:
        return 0

    puzzle[y][x] = '*'
    container.append((x, y))
    size = 1

    for dx, dy in ((0, 1), (1, 0), (0, -1), (-1, 0)):
        nx, ny = x + dx, y + dy
        if 0 <= nx < width and 0 <= ny < height:
            size += find_shape_and_size(nx, ny, c, container)

    return size


def get_perimeter(shape: List[List[int]]) -> int:
    perimeter = 0
    width, height = len(shape[0]), len(shape)

    for y, row in enumerate(shape):
        for x, c in enumerate(row):
            if c:
                for dx, dy in ((0, 1), (1, 0), (0, -1), (-1, 0)):
                    nx, ny = x + dx, y + dy
                    if not (0 <= nx < width and 0 <= ny < height):
                        perimeter += 1
                    elif not shape[ny][nx]:
                        perimeter += 1

    return perimeter


def get_sides(shape: List[List[bool]]) -> int:
    sides = 0
    rotated_shape = [[shape[y][x] for y in range(len(shape))] for x in range(len(shape[0]))]

    for shape in (shape, rotated_shape):
        empty_row = [False for _ in range(len(shape[0]))]
        shape.insert(0, empty_row)
        shape.append(empty_row)
        for upper, lower in pairwise(shape):
            last_situation = None
            for u, l in zip(upper, lower):
                situation = u, l
                if situation != last_situation and sum(situation) == 1:
                    sides += 1
                last_situation = situation

    return sides


def shape_list_to_shape(shape_list: List[Tuple[int, int]]) -> List[List[str]]:
    min_x, max_x, min_y, max_y = (
        min(x for x, y in shape_list),
        max(x for x, y in shape_list),
        min(y for x, y in shape_list),
        max(y for x, y in shape_list)
    )
    shape = [[False for _ in range(max_x - min_x + 1)] for _ in range(max_y - min_y + 1)]
    for sx, sy in shape_list:
        shape[sy - min_y][sx - min_x] = True
    return shape


puzzle = read_input(2024, 12)
width = puzzle.index('\n')
height = puzzle.count('\n') + 1
puzzle = list(map(list, batched(puzzle.replace('\n', ''), width)))
total_1 = total_2 = 0

for y, row in enumerate(puzzle):
    for x, c in enumerate(row):
        if c != '*':
            shape_list = []
            size = find_shape_and_size(x, y, c, shape_list)
            shape = shape_list_to_shape(shape_list)
            total_1 += size * get_perimeter(shape)
            total_2 += size * get_sides(shape)


print(total_1)
print(total_2)
