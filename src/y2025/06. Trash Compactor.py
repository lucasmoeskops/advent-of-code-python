#!/usr/bin/env python3

"""
AoC Day 6 - Trash Compactor - in Python.
"""

__author__ = "Lucas Moeskops"
__date__ = "2025-12-06"
__summary__ = "Trash Compactor"

from functools import reduce

from lib import read_input
from operator import add, mul

puzzle = read_input(2025, 6)
lines = puzzle.split('\n')
# Last line contains the operators
operators = lines.pop().split()
# Make an "excel-sheet" of numbers
number_grid = [[*map(int, line.split())] for line in lines]

grand_total_1 = 0
grand_total_2 = 0

# Check each column and calculate the total based on the operator
for i, column in enumerate(zip(*number_grid)):
    operator_function = add if operators[i] == '+' else mul
    initial = 0 if operators[i] == '+' else 1
    grand_total_1 += reduce(operator_function, column, initial)

# In part 2, the lines array acts as the "excel-sheet"
operator_index = 0
operator_function = add if operators[operator_index] == '+' else mul
amount = 0 if operators[operator_index] == '+' else 1

for i, column in enumerate(zip(*lines)):
    value = ''.join(column).replace(' ', '')

    if value:
        # If we found a number, add it to the current amount
        amount = operator_function(amount, int(value))
    else:
        # If we found only spaces, finalize the current amount and move to the next sum
        grand_total_2 += amount
        operator_index += 1
        operator_function = add if operators[operator_index] == '+' else mul
        amount = 0 if operators[operator_index] == '+' else 1

grand_total_2 += amount

print(grand_total_1)
print(grand_total_2)