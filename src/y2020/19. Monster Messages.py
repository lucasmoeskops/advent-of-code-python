#!/usr/bin/env python3

"""
AoC Day 19 - Monster Messages - in Python.

Recursive matching.
"""
 
__author__ = "Lucas Moeskops"
__date__ = "2020-12-19"

from functools import reduce
from itertools import chain
from sys import stdin

parse_option = lambda o: [(v[1:-1] if v[0] == '"' else int(v)) for v in o.split(' ')]

def parse_rule(rule):
    number, rest = rule.split(':')
    options = rest[1:].split(' | ')
    return (int(number), [parse_option(option) for option in options])

def match_rule(rules, rule, message):
    os, matching, fst = rules[rule], True, os[0][0]
    if isinstance(fst, str):
        if message.startswith(fst):
            yield message[len(fst):]
        return
    # Go through all options and yield those for which all values match consecutively
    for vs in os:
        yield from reduce(
            lambda bs, v: reduce(lambda p, b: chain(p, match_rule(rules, v, b)), bs, []),
            vs,
            [message])

valid_message = lambda r, m: '' in match_rule(r, 0, m)

rules_raw, messages_raw = stdin.read().split('\n\n')
rules = dict(parse_rule(rule) for rule in rules_raw.split('\n'))
messages = messages_raw.split("\n")

print(f'1: {sum(1 for m in messages if valid_message(rules, m))}')

new_rules = ['8: 42 | 42 8', '11: 42 31 | 42 11 31']
rules.update(dict(parse_rule(rule) for rule in new_rules))

print(f'2: {sum(1 for m in messages if valid_message(rules, m))}')