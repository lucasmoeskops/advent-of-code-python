"""
AoC Day 23 - Category Six.
"""

__author__ = "Lucas Moeskops"
__date__ = "2023-11-10"
__summary__ = "Rebuild the network after a \"Category Six\" disaster"

from lib import *
from sys import stdin
from y2019.computer import make_runner


def computer(address):
    in_, out = deque([address]), deque()
    runner = make_runner(program, in_, out)
    next(runner)
    while True:
        value = yield out
        out.clear()
        in_.extend(value)
        next(runner)


def run():
    network = [computer(i) for i in range(50)]
    [next(c) for c in network]
    output = [c.send([-1]) for c in network]
    nat_value = first_nat_value = last_value_sent = None
    while True:
        distribution = defaultdict(list)
        for queue in output:
            while queue:
                a, b, c, *queue = queue
                distribution[a].extend((b, c))
        if 255 in distribution:
            if nat_value is None:
                first_nat_value = distribution[255][-2:]
            nat_value = distribution[255][-2:]
        if not distribution:
            if nat_value == last_value_sent:
                return first_nat_value[1], nat_value[1]
            last_value_sent = nat_value
            distribution[0] = nat_value
        output = [c.send(distribution[i] or [-1]) for i, c in enumerate(network)]


puzzle = stdin.read()
program = [int(code) for code in puzzle.split(',')]
first_y_value, last_y_value = run()
print(f'{first_y_value}\n{last_y_value}')
