"""
AoC Day 1 - The Tyranny of the Rocket Equation - in Python.
"""

__author__ = "Lucas Moeskops"
__date__ = "2020-12-27"
__summary__ = "Leave in spaceship to find stranded Santa"

from sys import stdin


def fuel_requirement(n):
    return max(0, n // 3 - 2)


def fuel_requirement_rec(n):
    fr = fuel_requirement(n)
    return fr + fuel_requirement_rec(fr) if fr else 0


numbers = [int(line) for line in stdin.read().split('\n')]

fr_sum = sum(map(fuel_requirement, numbers))
print(fr_sum)

fr_sum_2 = sum(map(fuel_requirement_rec, numbers))
print(fr_sum_2)
