#!/usr/bin/env python3

"""
AoC Day 16 - Ticket Translation - in Python.

Simple row-based solver which seems good enough to solve the given
input. Contains a while loop but exits before entering a second loop
for my input.
"""

__author__ = "Lucas Moeskops"
__date__ = "2020-12-16"

from math import prod
from sys import stdin

range_splitter  = lambda s: tuple(int(v) for v in s.split('-'))
in_bounds       = lambda s, e, v: s <= v <= e
in_bounds_multi = lambda rs, v: any(in_bounds(*r, v) for r in rs)
invalid_values  = lambda rs, t: [v for v in t if not in_bounds_multi(rs, v)]

def parse_prop(prop):
    name, value_string = prop.split(': ')
    return name, [*map(range_splitter, value_string.split(' or '))]

parse_ticket = lambda t: tuple(int(v) for v in t.split(','))

def deduce(props, tickets):
    column_map, num_values = {}, len(tickets[0])
    column_options = [set(props.keys()) for _ in range(num_values)]
    def remove_option(column, name):
        if name in column_options[column]:
            column_options[column].remove(name)
            if len(column_options[column]) == 1:
                column_map[column] = found_field = column_options[column].pop()
                for other_column in range(num_values):
                    remove_option(other_column, found_field)
    while len(column_map) < num_values:
        for name, ranges in props.items():
            for ticket in tickets:
                for column, value in enumerate(ticket):
                    if name in column_options[column]:
                        if not in_bounds_multi(ranges, value):
                            remove_option(column, name)
    return column_map

prop_defs, mine, nearby = stdin.read().split('\n\n')
props = dict(parse_prop(d) for d in prop_defs.split('\n'))
tickets = [parse_ticket(n) for n in nearby.split('\n')[1:]]
all_ranges = [r for rs in props.values() for r in rs]
valid_tickets = [t for t in tickets if not invalid_values(all_ranges, t)]

print(f'1: {sum(sum(invalid_values(all_ranges, t)) for t in tickets)}')

mine = [int(v) for v in mine.split('\n')[1].split(',')]
ticket_map = deduce(props, valid_tickets + [mine])

print(f'2: {prod(mine[i] for i, n in ticket_map.items() if n.startswith("departure"))}')