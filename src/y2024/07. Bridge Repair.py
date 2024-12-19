#!/usr/bin/env python3

"""
AoC Day 7 - Bridge Repair - in Python.
"""

__author__ = "Lucas Moeskops"
__date__ = "2024-12-07"
__summary__ = "Back to year 2022, day 9"

from lib import add, mul, read_input


def concat(a, b):
    return int(str(a) + str(b))


def evaluator(operations):
    def evaluate(nums, answer):
        current = [nums[0]]
        for num in nums[1:]:
            new = []
            for c in current:
                for op in operations:
                    if (result := op(c, num)) <= answer:
                        new.append(result)
            current = new
        return answer in current
    return evaluate


puzzle = read_input(2024, 7)
calibration_basic = evaluator((add, mul))
calibration_advanced = evaluator((add, mul, concat))
result_basic = 0
result_advanced = 0

for line in puzzle.split('\n'):
    result, *operators = map(int, line.replace(':', '').split())
    if calibration_basic(operators, result):
        result_basic += result
    elif calibration_advanced(operators, result):
        result_advanced += result

print(result_basic)
print(result_basic + result_advanced)
