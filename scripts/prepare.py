#!/usr/bin/env python
import os

from datetime import date
from os import listdir, path
from sys import argv


def message_align(message, length, align_character='-', spacing=1, side_character=''):
    remaining = max(0, length - spacing * 2 - len(side_character) * 2 - len(message))
    left, right = remaining // 2 + remaining % 2, remaining // 2
    return f'{side_character}{align_character * left}{" "*spacing}{message}{" "*spacing}{align_character * right}{side_character}'


flags = [y for x in argv if x.startswith('-') for y in x[1:]]
rest = [x for x in argv if not x.startswith('-')]

year = int(rest[1]) % 100 if len(rest) > 1 else date.today().year % 100
year += 2000

folder_location = path.normpath(path.join(__file__, '..', '..', 'src', f'y{year}'))
if not path.exists(folder_location):
    os.makedirs(folder_location)

files_created = 0
already_existing = {name.split('.')[0] for name in listdir(folder_location) if name.endswith('.py')}

TEMPLATE = """
from sys import stdin

DATA = stdin.read().split('\\n')

# ns = [int(x) for x in DATA]
# xs = [''.join(c for c in x if '0' <= c <= '9' or c == ' ') for x in DATA]
# xs = [list(map(int, x.split(' ')) for x in xs]
# cs = {(int(line.split(',')[0]), int(line.split(',')[1])): line.split(':')[1] for line in DATA]
# ns = [tuple(map(int, pair.replace('-', ',').split(','))) for pair in PAIRS]
# ps = [tuple(map(int, findall(r'(-?\d+)', line))) for line in DATA]
# coords = defaultdict(
#     lambda: 'X',
#     [
#         ((x, y), val)
#         for y, row in enumerate(DATA.split('\\n'))
#         for x, val in enumerate(row)
#     ]
# )

"""

for day in range(1, 26):
    if str(day).zfill(2) in already_existing:
        continue
    script_location = path.join(folder_location, f'{0 if day < 10 else ""}{day}. XXX.py')
    if not path.exists(script_location):
        open(script_location, 'w').write(TEMPLATE.lstrip())
        files_created += 1


first_message = f'=== Advent of Code {year} ==='
typical_length = len(first_message)
print(message_align(first_message, typical_length, align_character='=', spacing=0))
print(message_align(f'Prepared {files_created} files!', typical_length, align_character=' ', side_character='|'))
print(message_align('', typical_length, align_character='=', spacing=0))
