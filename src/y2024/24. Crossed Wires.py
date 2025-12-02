#!/usr/bin/env python3

"""
AoC Day 24 - Crossed Wires - in Python.
"""

__author__ = "Lucas Moeskops"
__date__ = "2024-12-23"
__summary__ = "Back to year 2022, day 23"

from lib import *


def run():
    output = 0
    change = True

    while change:
        change = False

        for (a, op, b), out in lookup.items():
            if a in values and b in values:
                a = values[a]
                b = values[b]
                before = values.get(out, None)

                if op == 'AND':
                    values[out] = a & b
                elif op == 'OR':
                    values[out] = a | b
                elif op == 'XOR':
                    values[out] = a ^ b

                change = change or before is None or values[out] != before

    for i in range(46):
        if 'z' + str(i).zfill(2) in values:
            output = output + values['z' + str(i).zfill(2)] * 2**i

    return output


def verify():
    accumulated_rest_name = current_name = natural_rest_name = None
    swaps = []
    write_name = None

    for i in range(45):
        x_var = 'x' + str(i).zfill(2)
        y_var = 'y' + str(i).zfill(2)
        z_var = 'z' + str(i).zfill(2)
        if i > 1:
            accumulated_rest_name = lookup[current_name, 'AND', accumulated_rest_name]
            accumulated_rest_name = lookup[natural_rest_name, 'OR', accumulated_rest_name]
        current_name = lookup[x_var, 'XOR', y_var]
        natural_rest_name = lookup[x_var, 'AND', y_var]
        if i > 0:
            try:
                write_name = lookup[current_name, 'XOR', accumulated_rest_name]
            except KeyError:
                if (natural_rest_name, 'XOR', accumulated_rest_name) in lookup:
                    swaps.extend([current_name, natural_rest_name])
                    current_name, natural_rest_name = natural_rest_name, current_name
                    write_name = lookup[current_name, 'XOR', accumulated_rest_name]
        else:
            accumulated_rest_name = natural_rest_name
            write_name = current_name

        if write_name != z_var:
            swaps.extend([write_name, z_var])
            if natural_rest_name == z_var:
                natural_rest_name = write_name
            else:
                key = reverse_lookup[z_var]
                lookup[key] = write_name
                lookup[tuple(key[::-1])] = write_name

    return ','.join(sorted(swaps))


puzzle = read_input(2024, 24)

values, gates = puzzle.split('\n\n')
values = {k: int(v) for k, v in (line.split(': ') for line in values.split('\n'))}
lookup = {}
reverse_lookup = {}

for line in gates.split('\n'):
    a, op, b, _, out = line.split()
    lookup[a, op, b] = out
    lookup[b, op, a] = out
    reverse_lookup[out] = a, op, b

print(run())
print(verify())
