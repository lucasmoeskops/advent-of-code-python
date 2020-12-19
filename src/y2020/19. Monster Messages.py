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

def parse_rule(rule):
    number, rest = rule.split(':')
    options = rest[1:].split(' | ')
    return (number, [option.split(' ') for option in options])

def match_rule(rules, rule, message):
    os = rules[rule]
    fst = os[0][0]
    if fst[0] == '"':
        if message.startswith(fst[1:-1]):
            yield message[len(fst)-2:]
        return
    # Go through all options and yield those for which all values match consecutively
    for vs in os:
        yield from reduce(
            lambda bs, v: reduce(lambda p, b: chain(p, match_rule(rules, v, b)), bs, []),
            vs,
            [message])

valid_message = lambda r, m: '' in match_rule(r, '0', m)

rules_raw, messages_raw = stdin.read().split('\n\n')
rules = dict(parse_rule(rule) for rule in rules_raw.split('\n'))
messages = messages_raw.split("\n")

print(f'1: {sum(1 for m in messages if valid_message(rules, m))}')

new_rules = ['8: 42 | 42 8', '11: 42 31 | 42 11 31']
rules.update(dict(parse_rule(rule) for rule in new_rules))

print(f'2: {sum(1 for m in messages if valid_message(rules, m))}')