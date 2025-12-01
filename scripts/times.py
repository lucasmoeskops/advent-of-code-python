#!/usr/bin/env python
import re
from datetime import date
from io import StringIO
from importlib.util import spec_from_file_location, module_from_spec
from math import log
from os import listdir, path
import sys
from sys import argv
from time import time


try:
    import xtermcolor
except ModuleNotFoundError:
    xtermcolor = None


def adjectivized_number(number):
    if number == 1:
        return '1st'
    elif number == 2:
        return '2nd'
    elif number == 3:
        return '3rd'
    return f'{number}th'


def benchmark_colorize(benchmark, amount, s):
    if xtermcolor is None:
        return s
    amount *= 1000000
    if amount < 1:
        # diamond
        rgb = 0x0CF
    elif amount < 10:
        # gold
        rgb = 0xFFD700
    elif amount < 100:
        # platinum
        print('plat')
        rgb = 0xE5E4E2
    elif amount < 1000:
        # silver
        rgb = 0xC0C0C0
    elif amount < 10000:
        # bronze
        rgb = 0xCD7F32
    elif amount < 100000:
        # emerald
        rgb = 0x50C878
    elif amount < 1000000:
        # ruby
        rgb = 0xE0115F
    else:
        # sapphire
        rgb = 0x0F52BA
    # amount *= 1000000
    #
    # red_ness = 0xFF if amount > benchmark else 0
    # green_ness = max(0, min(0xFF, int((0.5 + (1 - log(max(1, amount)) / log(benchmark)) * .5) * 0xFF if amount < benchmark else int(0xFF - min(0xFF, 0xFF * amount / 25 / benchmark)))))
    # blue_ness = int(max(0, 1000 - min(1000, amount)) * 0xFF // 400)
    #
    # return xtermcolor.colorize(s, rgb=red_ness*0x10000+green_ness*0x100+blue_ness)
    return xtermcolor.colorize(s, rgb=rgb)


def human_time(amount):
    if amount < 0.001:
        return f'{int(amount * 1000000)} microseconds!'
    if amount < 0.01:
        return f'{int(amount * 10000)/10.0} milliseconds!'
    if amount < 1:
        return f'{int(amount * 1000)} milliseconds!'
    if amount < 10:
        return f'{int(amount * 10)/10.0} seconds!'
    return f'{int(amount)} seconds!'


def message_align(message, length, align_character='-', spacing=1, side_character='', max_length=None, align=0):
    if xtermcolor is not None:
        real_message, message = message, re.sub(r'\x1b\[\d+(;\d+)*m', '', message)
    else:
        real_message = message
    if max_length and len(message) + len(side_character) * 2 + spacing * 2 > max_length:
        message = message[:max_length-spacing*2-len(side_character)*2-3] + '...'
    remaining = max(0, length - spacing * 2 - len(side_character) * 2 - len(message))
    if align == -1:
        left, right = 0, remaining
    elif align == 1:
        left, right = remaining, 0
    else:
        left, right = remaining // 2 + remaining % 2, remaining // 2
    return f'{side_character}{align_character * left}{" "*spacing}{real_message}{" "*spacing}{align_character * right}{side_character}'


flags = [x[1:] for x in argv if x.startswith('-')]
rest = [x for x in argv if not x.startswith('-')]

year = int(rest[1]) % 100 if len(rest) > 1 else date.today().year % 100
year += 2000

error = module = script_name = script_path = ''
buffer = script_spec = None

widths = 6, 40, 20, 4, 60
first_message = f'Advent of Code {year} runtimes'
print(message_align(first_message, sum(widths)+2*len(widths), align_character='='))
total_runtime = 0

benchmark = 1
num_puzzles = 25
per_puzzle = benchmark * 1000000 // num_puzzles

for day in range(1, 26):
    if date(year, 12, day) > date.today():
        continue

    day_message = message_align(f'Day {" " if day < 10 else ""}{day}', widths[0], align_character=' ', align=-1)
    title_message = message_align(' ', widths[1], side_character='|', align_character=' ')
    runtime_message = message_align(' ', widths[2], align_character=' ')
    success_message = message_align(' | ', widths[3], align_character=' ')
    lore_message = message_align('|  ', widths[4] + 2, align=-1, align_character=' ')

    data_location = path.normpath(path.join(__file__, '..', '..', 'data', f'y{year}', f'{str(day).zfill(2)}.txt'))
    test_location = path.normpath(path.join(__file__, '..', '..', 'data', f'y{year}', f'{str(day).zfill(2)}.out'))

    if not path.exists(data_location):
        # print(message_align(day_message + title_message + runtime_message + success_message, sum(widths), align_character=' ',
        #                     side_character='|'))
        continue

    data = open(data_location, 'r').read()
    while data.endswith('\n'):
        data = data[:-1]

    script_location = path.normpath(path.join(__file__, '..', '..', 'src', f'y{year}'))
    script_options = [x for x in listdir(script_location) if x.startswith(f'{str(day).zfill(2)}.') and x.endswith('.py')]
    if not script_options:
        print(message_align(day_message + title_message + runtime_message + lore_message, sum(widths), align_character=' ', side_character='|'))
        continue
    else:
        script_name = script_options[0]
        script_path = path.join(script_location, script_name)
        script_spec = spec_from_file_location('script', script_path)
        module = module_from_spec(script_spec)

    title = script_name[4:-3]
    title_message = message_align(title, widths[1], side_character='|', align_character=' ', max_length=widths[1], align=-1)

    sys.stdin = StringIO(data)

    buffer = StringIO()
    sys.stdout = buffer
    start = time()
    script_spec.loader.exec_module(module)
    end = time()
    sys.stdout = sys.__stdout__
    sys.stdin = sys.__stdin__
    buffer.seek(0)
    result = buffer.read().strip()
    test = open(test_location, 'r').read().strip() if path.exists(test_location) else None
    success = None if not path.exists(test_location) else result == test
    if success is False:
        print(result.replace('\n', '***'), test.replace('\n', '***'), sep='\n')

    runtime_message = benchmark_colorize(per_puzzle, end-start, human_time(end-start))
    runtime_message = message_align(' ' + runtime_message + ' ', widths[2], align_character=' ', max_length=widths[2], align=1, spacing=0)
    success_message = '✓' if success else ('✗' if success is False else '?')
    success_message = message_align(' | ' + success_message, widths[3], align_character=' ', max_length=widths[3])
    if hasattr(module, '__summary__'):
        lore_message = '| ' + module.__summary__[:60]
        lore_message += ' ' * (widths[4] - len(lore_message)+2)
    print(message_align(day_message + title_message + runtime_message + success_message + lore_message, sum(widths), align_character=' ', side_character='|'))

    total_runtime += end-start

print(message_align('', sum(widths)+2*len(widths), align_character='-', spacing=0))
total_message = message_align(f'Total runtime:', widths[0] + widths[1], align_character=' ', spacing=0, align=-1)
runtime_message = benchmark_colorize(1000000, total_runtime, human_time(total_runtime))
runtime_message = message_align(runtime_message, widths[2], align_character=' ', spacing=0, align=1)
print(message_align(total_message + ' ' + runtime_message, sum(widths)+2*len(widths), align_character=' ', side_character='|', spacing=0))
print(message_align('', sum(widths)+2*len(widths), align_character='=', spacing=0))
