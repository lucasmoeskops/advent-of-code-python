#!/usr/bin/env python3

"""
AoC Day 4 - Password Processing - in Python.

Pattern matching!
"""

__author__ = "Lucas Moeskops"
__date__ = "2020-12-04"

from re import match
from sys import stdin

required = {'byr', 'iyr', 'eyr', 'hgt', 'hcl', 'ecl', 'pid'}

validation = {
    'byr': lambda v: match(r'\d+$', v) and 1920 <= int(v) <= 2002,
    'iyr': lambda v: match(r'\d+$', v) and 2010 <= int(v) <= 2020,
    'eyr': lambda v: match(r'\d+$', v) and 2020 <= int(v) <= 2030,
    'hgt': (lambda v:
        match(r'\d+(cm|in)$', v)
         and 59 <= int(v[:-2]) <= 76 if v[-2:] == 'in' else 150 <= int(v[:-2]) <= 193),
    'hcl': lambda v: match(r'#[0-9a-f]{6}$', v),
    'ecl': lambda v: v in {'amb', 'blu', 'brn', 'gry', 'grn', 'hzl', 'oth'},
    'pid': lambda v: match(r'\d{9}$', v),
}

keys = lambda passport: (item[-3:] for item in passport.split(':')[:-1])

keys_values = lambda passport: (tuple(kv.split(':')) for kv in passport.split())

fields_valid = lambda passport: not required - set(keys(passport))

values_valid = (lambda passport:
    all(validation[key](value)
        for key, value in keys_values(passport)
        if key in required))

all_valid = lambda passport: fields_valid(passport) and values_valid(passport)

passports = stdin.read().split('\n\n')

print(f'1: {len([*filter(fields_valid, passports)])}')
print(f'2: {len([*filter(all_valid, passports)])}')
