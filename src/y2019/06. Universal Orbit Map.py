"""
AoC Day 6 - Universal Orbit Map - in Python.
"""

__author__ = "Lucas Moeskops"
__date__ = "2020-12-28"

from collections import Counter, defaultdict, deque
from sys import stdin

def make_orbit_map(orbits):
    m = defaultdict(list)
    for x, y in orbits:
        m[x].append(y)
    return m

def make_orbit_count_map(orbit_map):
    m = Counter()
    queue = deque([('COM', 0)])
    while queue:
        item, d = queue.popleft()
        for orbit in orbit_map[item]:
            m[orbit] = d + 1
            queue.append((orbit, d + 1))
    return m

def make_inverse_orbit_map(orbits):
    return {y: x for x, y in orbits}

def ancestors(inverse_map, orbit):
    parent = inverse_map.get(orbit)
    return [parent] + (ancestors(inverse_map, parent) if parent else [])

orbits = [o.split(')') for o in stdin.read().split('\n')]

map_ = make_orbit_map(orbits)
print(f'1: {sum(make_orbit_count_map(map_).values())}')

map_ = make_inverse_orbit_map(orbits)
you_path = set(ancestors(map_, "YOU"))
santa_path = set(ancestors(map_, "SAN"))
print(f'2: {len((you_path | santa_path) - (you_path & santa_path))}')