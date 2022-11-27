from collections import defaultdict
from sys import stdin

lines = stdin.read().split('\n')
instructions = [tuple(line.split(' ')) for line in lines]
registers = defaultdict(int)
functions = {
    '>=': int.__ge__,
    '<=': int.__le__,
    '>': int.__gt__,
    '<': int.__lt__,
    '==': int.__eq__,
    '!=': int.__ne__,
}
actions = {
    'inc': int.__add__,
    'dec': int.__sub__,
}

highest_ever = 0
for target, action, amount, _, condition_target, function, check_amount in instructions:
    if functions[function](registers[condition_target], int(check_amount)):
        registers[target] = actions[action](registers[target], int(amount))
        highest_ever = max(registers[target], highest_ever)

print(max(registers.values()))
print(highest_ever)
