from sys import stdin
from functools import reduce

questions_any = lambda group: len(set(''.join(group.replace('\n', ''))))

questions_all = lambda group: len(reduce(set.__and__, map(set, group.split('\n'))))

groups = stdin.read().split('\n\n')

print(f'1: {sum(map(questions_any, groups))}')
print(f'2: {sum(map(questions_all, groups))}')
