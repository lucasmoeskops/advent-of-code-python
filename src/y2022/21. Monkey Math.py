from collections import defaultdict
from copy import copy
from re import findall
from sys import stdin

DATA = stdin.read().split('\n')


def op(operation, lhs, rhs):
    match operation[5]:
        case '*':
            return lhs * rhs
        case '/':
            return lhs // rhs
        case '+':
            return lhs + rhs
    return lhs - rhs


def reverse_op(operation, target, known_is_right):
    match operation[5]:
        case '*':
            return lambda x: target // x
        case '/':
            return (lambda x: target * x) if known_is_right else (lambda x: target // x)
        case '+':
            return lambda x: target - x
    return (lambda x: target + x) if known_is_right else (lambda x: x - target)


def solve(monkeys, monkey_deps):
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


def do():
    monkeys = {}
    monkey_deps = {}

    for line in DATA:
        monkey, operation = line.replace(':', '').split(' ', maxsplit=1)
        monkeys[monkey] = operation
        monkey_deps[monkey] = tuple(findall(r'[a-z]{4}', operation))

    print(solve(monkeys, copy(monkey_deps))['root'])

    monkey_deps['humn'] = ('you',)
    monkey_vals = solve(monkeys, monkey_deps)
    lhs, rhs = monkey_deps['root']

    queue = [(lhs, monkey_vals[rhs])] if monkey_vals[lhs] is None else [(rhs, monkey_vals[lhs])]

    while queue:
        item, target = queue.pop()

        if item == 'humn':
            print(target)
            break

        lhs, rhs = monkey_deps[item]
        known, unknown = (rhs, lhs) if monkey_vals[lhs] is None else (lhs, rhs)
        reverse_engineer = reverse_op(monkeys[item], target, known == rhs)
        queue.append((unknown, reverse_engineer(monkey_vals[known])))


do()
