"""
AoC Day 14 - The Space Stoichiometry.
"""

__author__ = "Lucas Moeskops"
__date__ = "2023-11-05"
__summary__ = "Make new fuel from the material of the rings of Saturn"

from lib import *
from sys import stdin


def costs_of(amount, thing, stores=None):
    if thing == 'ORE':
        return amount

    costs = 0

    if stores is None:
        stores = defaultdict(int)

    obtain, reqs = formulas[thing]
    multiplier = 1

    while stores[thing] < amount:
        for req, n in reqs.items():
            n *= multiplier
            available = stores[req]
            take = min(n, available)
            n -= take
            stores[req] -= take

            if n:
                costs += costs_of(n, req, stores)

        stores[thing] += obtain

    stores[thing] -= amount
    return costs


formulas_raw = stdin.read().split('\n')
formulas = {}

for formula in formulas_raw:
    reqs, out = formula.split(' => ')
    reqs = [req.split(' ') for req in reqs.split(', ')]
    reqs = {k: int(v) for v, k in reqs}
    amount, thing = out.split(' ')
    assert thing not in formulas, 'Duplicate formula!'
    formulas[thing] = (int(amount), reqs)


def transform(costs, state):
    costs += costs_of(1, 'FUEL', state)
    # print(costs)
    return costs, state


def make_store():
    stores = {thing: 0 for thing in formulas}
    stores['ORE'] = 0
    return stores


store = make_store()
costs = costs_of(1, 'FUEL', store)
costs_1 = costs
print(costs)
print('?')

# start, length = period(transform_stream(curry(transform), (0, make_store())), key=lambda s: tuple(s[1].values()))
length = 0, 5000

# # costs, stores = nth(transform_stream(curry(transform), (0, make_store())), 999)
# costs, stores, rounds = 0, make_store(), 0
# while max(stores.values()) >= 6 or 1000000000000 / max(1, costs) > 1250 or rounds < 100:
#     rounds += 1
#     costs, stores = transform(costs, stores)

# # print(state)
# # state, rounds = until(curry(transform), state, lambda old, new: max(new[1].values()) < 5)
# # costs, stores = state
# length = rounds
# print(length, costs, sum(stores.values()))

# # costs, stores = nth(transform_stream(curry(transform), (0, make_store())), length)
# base, remaining = divmod(1000000000000 - fuel_1 * 1000, costs)
# fuel = base * length
# for k in stores:
#     stores[k] *= base
# remaining += fuel_1 * 1000
# while remaining > 0:
#     remaining -= costs_of(1, 'FUEL', stores)
#
#     if remaining >= 0:
#         fuel += 1
#
# print(fuel)
# print(stores)
