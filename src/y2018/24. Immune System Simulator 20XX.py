from lib import *
from sys import stdin

IMMUNE = 'immune'
INFECTION = 'infection'

READER = r'(?P<units>\d+) units each with (?P<hit_points>\d+) hit points (\(((weak to (?P<weak>((\w+)(, )?)+))?(; )?(immune to (?P<immune>((\w+)(, )?)+))?)+\) )?with an attack that does (?P<damage>\d+) (?P<damage_type>\w+) damage at initiative (?P<initiative>\d+)'

immune, infection = stdin.read().split('\n\n')
groups = []

for side, system in zip((IMMUNE, INFECTION), (immune, infection)):
    system = system.split('\n')[1:]
    system = [*map(lambda s: s.groupdict() if s else '?', map(re.compile(READER).match, system))]

    for subsystem in system:
        subsystem['units'] = int(subsystem['units'])
        subsystem['hit_points'] = int(subsystem['hit_points'])
        subsystem['damage'] = int(subsystem['damage'])
        subsystem['initiative'] = int(subsystem['initiative'])
        subsystem['weak'] = subsystem['weak'].split(', ') if subsystem['weak'] else []
        subsystem['immune'] = subsystem['immune'].split(', ') if subsystem['immune'] else []
        subsystem['side'] = side
        groups.append(subsystem)


def effective_power(group):
    return group['damage'] * group['units']


def round(groups):
    initiatives = [group['initiative'] for group in groups]
    ordered = sorted(range(len(groups)), key=initiatives.__getitem__, reverse=True)
    powers = [effective_power(group) for group in groups]
    ordered = sorted(ordered, key=powers.__getitem__, reverse=True)
    targets = {}

    for i in ordered:
        group = groups[i]
        ep = effective_power(group)
        best = 0
        best_at = None
        best_ep = None
        best_init = None

        for j, other_group in enumerate(groups):
            if other_group['side'] == group['side']:
                continue

            if j in targets.values():
                continue

            if group['damage_type'] in other_group['immune']:
                continue

            damage_dealt = ep

            if group['damage_type'] in other_group['weak']:
                damage_dealt *= 2

            if damage_dealt > best:
                best = damage_dealt
                best_at = j
                best_ep = effective_power(other_group)
                best_init = other_group['initiative']
            elif damage_dealt == best:
                other_ep = effective_power(other_group)
                if other_ep > best_ep:
                    best_at = j
                    best_ep = other_ep
                    best_init = other_group['initiative']
                elif other_ep == best_ep:
                    if other_group['initiative'] > best_init:
                        best_at = j
                        best_init = other_group['initiative']

        if best_at is not None:
            targets[i] = best_at

    attack_order = sorted(range(len(groups)), key=initiatives.__getitem__, reverse=True)
    casualties = 0

    for i in attack_order:
        if i not in targets:
            continue

        group = groups[i]
        damage = effective_power(group)
        defending_group = groups[targets[i]]

        if group['damage_type'] in defending_group['weak']:
            damage *= 2

        units_lost = damage // defending_group['hit_points']
        casualties += units_lost
        defending_group['units'] = max(0, defending_group['units'] - units_lost)

    if not casualties:
        return []

    return [group for group in groups if group['units'] > 0]


def resolve(groups):
    groups, _ = until(round, groups, lambda old, new: len(set(map(itemgetter('side'), old))) <= 1)
    winner = groups[0]['side'] if groups else 'INBETWEEN'
    return groups, winner


def apply_boost(groups, amount):
    for group in groups:
        if group['side'] == IMMUNE:
            group['damage'] += amount


def try_boost(amount):
    groups_copy = deepcopy(groups)
    apply_boost(groups_copy, amount)
    groups_copy, winner = resolve(groups_copy)
    return winner


groups_copy = deepcopy(groups)
remains, _ = resolve(groups_copy)
print(sum(map(itemgetter('units'), remains)))

minimal_boost = bisect_left(range(1, 1000), IMMUNE, key=try_boost) + 1
apply_boost(groups, minimal_boost)
remains, _ = resolve(groups)
print(sum(map(itemgetter('units'), remains)))
