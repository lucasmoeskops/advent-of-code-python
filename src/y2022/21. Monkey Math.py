from collections import defaultdict
from copy import copy
from re import findall, sub
from sys import stdin

DATA = stdin.read().split('\n')


def op(operation, lhs, rhs):
    if '*' in operation:
        return lhs * rhs
    if '/' in operation:
        return lhs // rhs
    if '+' in operation:
        return lhs + rhs
    return lhs - rhs


def reverse_op(operation, target, known_is_right):
    if '*' in operation:
        return lambda x: target // x
    if '/' in operation:
        if known_is_right:
            return lambda x: target * x
        else:
            return lambda x: target // x
    if '+' in operation:
        return lambda x: target - x
    if '-' in operation:
        if known_is_right:
            return lambda x: target + x
        else:
            return lambda x: x - target


def solve(monkey_deps):
    monkey_vals = defaultdict(lambda: None)

    change = True
    while change and monkey_deps:
        change = False
        for monkey in list(monkey_deps):
            if monkey not in monkey_deps:
                continue
            if not monkey_deps[monkey]:
                monkey_vals[monkey] = int(monkeys[monkey])
                del monkey_deps[monkey]
                change = True
                continue
            for dep in monkey_deps[monkey]:
                if monkey_vals[dep] is None:
                    break
            else:
                monkey_vals[monkey] = op(monkeys[monkey], *map(monkey_vals.__getitem__, monkey_deps[monkey]))
                del monkey_deps[monkey]
                change = True

    return monkey_vals


monkeys = {}
monkey_deps = {}

for line in DATA:
    monkey, operation = line.replace(':', '').split(' ', maxsplit=1)
    monkeys[monkey] = operation
    monkey_deps[monkey] = tuple(findall(r'[a-z]{4}', operation))

print(solve(copy(monkey_deps))['root'])

monkey_deps['humn'] = ('you',)
monkey_vals = solve(monkey_deps)
lhs, rhs = monkey_deps['root']

if monkey_vals[lhs] is None:
    queue = [(lhs, monkey_vals[rhs])]
else:
    queue = [(rhs, monkey_vals[lhs])]

while queue:
    item, target = queue.pop()

    if item == 'humn':
        print(target)
        break

    lhs, rhs = monkey_deps[item]

    if monkey_vals[lhs] is None:
        if monkey_vals[rhs] is None:
            print('I don\'t know how to fix yet!')
        else:
            queue.append((lhs, reverse_op(monkeys[item], target, True)(monkey_vals[rhs])))
    else:
        queue.append((rhs, reverse_op(monkeys[item], target, False)(monkey_vals[lhs])))
