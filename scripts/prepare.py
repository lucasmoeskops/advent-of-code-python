#!/usr/bin/env python
import os

import requests
import subprocess
import webbrowser
from datetime import date
from io import StringIO
from importlib.util import spec_from_file_location, module_from_spec
from os import listdir, path
import sys
from shutil import which
from sys import argv, stdin
from time import time


def message_align(message, length, align_character='-', spacing=1, side_character=''):
    remaining = max(0, length - spacing * 2 - len(side_character) * 2 - len(message))
    left, right = remaining // 2 + remaining % 2, remaining // 2
    return f'{side_character}{align_character * left}{" "*spacing}{message}{" "*spacing}{align_character * right}{side_character}'


flags = [y for x in argv if x.startswith('-') for y in x[1:]]
rest = [x for x in argv if not x.startswith('-')]

year = int(rest[1]) % 100 if len(argv) > 1 else date.today().year % 100
year += 2000

folder_location = path.normpath(path.join(__file__, '..', '..', 'src', f'y{year}'))
if not path.exists(folder_location):
    os.makedirs(folder_location)

files_created = 0
already_existing = {name.split('.')[0] for name in listdir(folder_location) if name.endswith('.py')}

for day in range(1, 26):
    if str(day).zfill(2) in already_existing:
        continue
    script_location = path.join(folder_location, f'{0 if day < 10 else ""}{day}. XXX.py')
    if not path.exists(script_location):
        open(script_location, 'w').write('from sys import stdin\n\nDATA = stdin.read()\n\n')
        files_created += 1


first_message = f'=== Advent of Code {year} ==='
typical_length = len(first_message)
print(message_align(first_message, typical_length, align_character='=', spacing=0))
print(message_align(f'Prepared {files_created} files!', typical_length, align_character=' ', side_character='|'))
print(message_align('', typical_length, align_character='=', spacing=0))