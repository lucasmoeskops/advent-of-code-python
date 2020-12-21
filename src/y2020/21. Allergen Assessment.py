#!/usr/bin/env python3

"""
AoC Day 21 - Allergen Assessment - in Python.

A bit like Day 16.
"""

__author__ = "Lucas Moeskops"
__date__ = "2020-12-21"

from operator import itemgetter
from re import match
from sys import stdin

re_contents = r'([\w ]*\w)( \(contains ([\w, ]+)\))?'

def parse_contents(line):
    contents, _, allergens = match(re_contents, line).groups()
    return contents.split(' '), (allergens.split(', ') if allergens else [])    

def build_allergen_map(contentss):
    allergen_map = {}
    for is_, as_ in contentss:
        for a in as_:
            if a in allergen_map:
                allergen_map[a] &= set(is_)
            else:
                allergen_map[a] = set(is_)
    return allergen_map

def get_allergenless_count(contentss, allergen_map):
    allergenic_ingredients = {i for s in allergen_map.values() for i in s}
    return sum(1 for c in contentss for i in c[0] if i not in allergenic_ingredients)

def map_to_ingredients(allergen_map):
    to_ingredients = {}
    for _ in range(len(allergen_map)):
        for a, s in allergen_map.items():
            if len(s) == 1:
                to_ingredients[a] = i = s.pop()
                [t.remove(i) for t in allergen_map.values() if i in t]
    return to_ingredients

lines = stdin.read().split('\n')
contentss = [parse_contents(line) for line in lines]
ingredients = {i for c in contentss for i in c[0]}
allergen_map = build_allergen_map(contentss)

print(f'1: {get_allergenless_count(contentss, allergen_map)}')

by_allergen = sorted(map_to_ingredients(allergen_map).items(), key=itemgetter(0))
print(f'2: {",".join(i for a, i in by_allergen)}')