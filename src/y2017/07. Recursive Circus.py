from collections import Counter
from functools import reduce
from operator import itemgetter
from sys import stdin

lines = stdin.read().split('\n')

programs = {line.split(' ')[0]: (int(line.split(' ')[1][1:-1]), {*filter(None, (line+'-> ').split('-> ')[1].split(', '))}) for line in lines}
supporteds = set(reduce(set.__or__, map(itemgetter(1), programs.values())))
root_program = list(set(programs) - supporteds)[0]
print(root_program)


def find_required_weight(program_):
    weight, sub_programs = programs[program_]
    sub_weights = {sub: find_required_weight(sub) for sub in programs[program_][1]}
    counter = Counter(sub_weights.values())
    if sub_weights and len(counter) != 1:
        right, wrong = map(itemgetter(0), counter.most_common())
        if wrong < 0:
            return wrong
        difference = wrong - right
        wrong_program = [p for p, w in sub_weights.items() if w == wrong][0]
        return -(programs[wrong_program][0] - difference)
    return weight + sum(sub_weights.values())

print(-find_required_weight(root_program))
