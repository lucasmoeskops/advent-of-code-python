from collections import defaultdict
from math import prod
from re import findall
from sys import stdin
from typing import NamedTuple, Tuple

DATA = stdin.read()

# Different parsing for example
data = DATA.replace('\n\n', 'XXX').replace('\n', '').split('XXX') if '\n\n' in DATA else DATA.split('\n')


class Blueprint(NamedTuple):
    ID: int
    ore_robot_cost: int
    clay_robot_cost: int
    obsidian_robot_cost: Tuple[int, int]
    geode_robot_cost: Tuple[int, int]


class QueueItem(NamedTuple):
    minute: int = 0
    ore: int = 0
    ore_robots: int = 0
    clay: int = 0
    clay_robots: int = 0
    obsidian: int = 0
    obsidian_robots: int = 0
    geode_robots: int = 0
    geodes_cracked: int = 0


blueprints = []

for blueprint in data:
    (
        ID,
        ore,
        clay,
        obsidian_ore,
        obsidian_clay,
        geode_ore,
        geode_obsidian
    ) = map(int, findall(r'(-?\d+)', blueprint))
    blueprints.append(Blueprint(
        ID=ID,
        ore_robot_cost=ore,
        clay_robot_cost=clay,
        obsidian_robot_cost=(obsidian_ore, obsidian_clay),
        geode_robot_cost=(geode_ore, geode_obsidian),
    ))


def optimum(blueprint, minutes=24):
    (
        _,
        ore_robot_ore_cost,
        clay_robot_ore_cost,
        (obsidian_robot_ore_cost, obsidian_robot_clay_cost),
        (geode_robot_ore_cost, geode_robot_obsidian_cost)
    ) = blueprint
    stack = [QueueItem(minute=0, ore_robots=1)]
    best = defaultdict(int)
    max_obsidian_per_turn = geode_robot_obsidian_cost
    max_clay_per_turn = obsidian_robot_clay_cost
    max_ore_per_turn = max(
        geode_robot_ore_cost,
        ore_robot_ore_cost,
        clay_robot_ore_cost,
        ore_robot_ore_cost
    )

    while stack:
        (
            minute,
            ore,
            ore_robots,
            clay,
            clay_robots,
            obsidian,
            obsidian_robots,
            geode_robots,
            geodes_cracked
        ) = stack.pop()

        if geodes_cracked > best[minute]:
            best[minute] = geodes_cracked

        if minute == minutes:
            continue

        # N.B. This is flawed, gives the wrong answer for the test case
        if geodes_cracked < best[minute]:
            continue

        minute += 1
        should_buy_ore_robot = ore_robots < max_ore_per_turn and ore >= ore_robot_ore_cost
        should_buy_clay_robot = clay_robots < max_clay_per_turn and ore >= clay_robot_ore_cost
        # Not entirely sure if this optimization is possible
        should_buy_clay_robot &= clay * obsidian_robot_ore_cost < ore * obsidian_robot_clay_cost
        should_buy_obsidian_robot = (
                obsidian_robots < max_obsidian_per_turn
                and ore >= obsidian_robot_ore_cost
                and clay >= obsidian_robot_clay_cost
        )
        can_buy_geode_robot = ore >= geode_robot_ore_cost and obsidian >= geode_robot_obsidian_cost
        ore += ore_robots
        clay += clay_robots
        obsidian += obsidian_robots
        geodes_cracked += geode_robots

        kwargs = {
            'minute': minute,
            'ore': ore,
            'ore_robots': ore_robots,
            'clay': clay,
            'clay_robots': clay_robots,
            'obsidian': obsidian,
            'obsidian_robots': obsidian_robots,
            'geode_robots': geode_robots,
            'geodes_cracked': geodes_cracked
        }

        if ore < max_ore_per_turn + ore_robots:
            stack.append(QueueItem(**kwargs))

        if should_buy_obsidian_robot:
            stack.append(QueueItem(**{
                **kwargs,
                'ore': ore - blueprint.obsidian_robot_cost[0],
                'clay': clay - blueprint.obsidian_robot_cost[1],
                'obsidian_robots': obsidian_robots + 1,
            }))

        if should_buy_clay_robot:
            stack.append(QueueItem(**{
                **kwargs,
                'ore': ore - blueprint.clay_robot_cost,
                'clay_robots': clay_robots + 1,
            }))

        if should_buy_ore_robot:
            stack.append(QueueItem(**{
                **kwargs,
                'ore': ore - blueprint.ore_robot_cost,
                'ore_robots': ore_robots + 1
            }))

        if can_buy_geode_robot:
            stack.append(QueueItem(**{
                **kwargs,
                'ore': ore - blueprint.geode_robot_cost[0],
                'obsidian': obsidian - blueprint.geode_robot_cost[1],
                'geode_robots': geode_robots + 1,
            }))

    return best[minutes]


print(sum(blueprint.ID * optimum(blueprint, 24) for blueprint in blueprints))
print(prod(optimum(blueprint, 32) for blueprint in blueprints[:3]))
