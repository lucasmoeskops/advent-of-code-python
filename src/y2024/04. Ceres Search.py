#!/usr/bin/env python3

"""
AoC Day 4 - Ceres Search - in Python.
"""

__author__ = "Lucas Moeskops"
__date__ = "2024-12-04"
__summary__ = "Back to year 2019, day 10"

from lib import re, read_input


puzzle = read_input(2024, 4)
puzzle += '\n'
w = puzzle.index('\n')
regexes = [
    r'XMAS',
    r'SAMX',
    rf'X(?=.{{{w}}}M.{{{w}}}A.{{{w}}}S)',
    rf'S(?=.{{{w}}}A.{{{w}}}M.{{{w}}}X)',
    rf'X(?=.{{{w+1}}}M.{{{w+1}}}A.{{{w+1}}}S)',
    rf'S(?=.{{{w+1}}}A.{{{w+1}}}M.{{{w+1}}}X)',
    rf'X(?=.{{{w-1}}}M.{{{w-1}}}A.{{{w-1}}}S)',
    rf'S(?=.{{{w-1}}}A.{{{w-1}}}M.{{{w-1}}}X)',
]
print(sum(len(re.findall(regex, puzzle, re.M|re.DOTALL)) for regex in regexes))
regexes = [
    rf'M(?=.M.{{{w-1}}}A.{{{w-1}}}S.S)',
    rf'M(?=.S.{{{w-1}}}A.{{{w-1}}}M.S)',
    rf'S(?=.S.{{{w-1}}}A.{{{w-1}}}M.M)',
    rf'S(?=.M.{{{w-1}}}A.{{{w-1}}}S.M)',
]
print(len(re.findall('|'.join(regexes), puzzle, re.M|re.DOTALL)))
