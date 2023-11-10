"""
AoC Day 2 - 1202 Program Alarm - in Python.
"""

__author__ = "Lucas Moeskops"
__date__ = "2020-12-27"
__summary__ = "Repair computer for gravity assist around Moon"

from sys import stdin


def run(program):
    position, program, halted = 0, program[:], False
    while not halted:
        state = position, program, halted
        position, program, halted = step(state)
    return program


def step(state):
    position, program, halted = state
    operation = program[position]
    if operation == 1:
        _, a, b, c, *_ = program[position:]
        program[c] = program[a] + program[b]
        position += 4
    elif operation == 2:
        _, a, b, c, *_ = program[position:]
        program[c] = program[a] * program[b]
        position += 4
    elif operation == 99:
        position += 1
        halted = True
    return position, program, halted


def initialize(program, noun, verb):
    program[1] = noun
    program[2] = verb


def find_19690720(program):
    for noun in range(100):
        for verb in range(100):
            program_2 = program[:]
            initialize(program_2, noun, verb)
            if run(program_2)[0] == 19690720:
                return noun, verb


program = [int(line) for line in stdin.read().split(',')]

program_2 = program[:]
initialize(program_2, 12, 2)
result = run(program_2)
print(result[0])

noun, verb = find_19690720(program)
print(noun * 100 + verb)
