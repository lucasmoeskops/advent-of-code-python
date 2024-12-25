#!/usr/bin/env python3

"""
AoC Day 17 - Chronospatial Computer - in Python.
"""

__author__ = "Lucas Moeskops"
__date__ = "2024-12-17"
__summary__ = "Back to year 2018, day 6"

from lib import re, read_input


def make_clone(index=None, prefix=0):
    # Assume always three bits are shifted until the end
    if index is None:
        index = len(program) - 1

    for i in range(8):
        if run(prefix + i) == program[index:]:
            if index == 0:
                return prefix + i

            if answer := make_clone(index - 1, prefix * 8 + i * 8):
                return answer


def run(value):
    registry = [value, 0, 0]
    out = []
    pointer = 0

    while pointer < len(program):
        instruction = program[pointer]
        value = program[pointer+1]
        combo_value = value
        if 4 <= value <= 6:
            combo_value = registry[value - 4]
        if instruction == 0:  # adv
            registry[0] = registry[0] // (2 ** combo_value)
        elif instruction == 1:  # bxl
            registry[1] = registry[1] ^ value
        elif instruction == 2:  # bst
            registry[1] = combo_value % 8
        elif instruction == 3:  # jnz
            if registry[0] != 0:
                pointer = value
                continue
        elif instruction == 4:  # bxc
            registry[1] = registry[1] ^ registry[2]
        elif instruction == 5:  # out
            out.append(combo_value % 8)
        if instruction == 6:  # bdv
            registry[1] = registry[0] // (2 ** combo_value)
        if instruction == 7:  # cdv
            registry[2] = registry[0] // (2 ** combo_value)
        pointer += 2

    return out


puzzle = read_input(2024, 17)
a_value, _, _, *program = map(int, re.findall(r'-?\d+', puzzle + '*'))
print(','.join(map(str, run(a_value))))
print(make_clone())
