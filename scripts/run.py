#!/usr/bin/env python
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


def adjectivized_number(number):
    if number == 1:
        return '1st'
    elif number == 2:
        return '2nd'
    elif number == 3:
        return '3rd'
    return f'{number}th'


def human_time(amount):
    if amount < 0.001:
        return f'{int(amount * 1000000)} microseconds!'
    if amount < 1:
        return f'{int(amount * 1000)} milliseconds!'
    if amount < 10:
        return f'{int(amount * 10)/10.0} seconds!'
    return f'{int(amount)} seconds!'


def message_align(message, length, align_character='-', spacing=1, side_character=''):
    remaining = max(0, length - spacing * 2 - len(side_character) * 2 - len(message))
    left, right = remaining // 2 + remaining % 2, remaining // 2
    return f'{side_character}{align_character * left}{" "*spacing}{message}{" "*spacing}{align_character * right}{side_character}'


flags = [y for x in argv if x.startswith('-') for y in x[1:]]
rest = [x for x in argv if not x.startswith('-')]

day = int(rest[1]) if len(rest) > 1 else date.today().day
year = int(rest[2]) % 100 if len(rest) > 2 else date.today().year % 100
year += 2000

direct = 'd' in flags
interactive = 'i' in flags
copy_to_pasteboard = 'p' in flags
do_submit = 's' in flags

if direct and copy_to_pasteboard:
    print('Direct and copy to pasteboard can not be used together')
    quit()
if direct and do_submit:
    print('Direct and submit can not be used together')
    quit()
if interactive and do_submit:
    print('Interactive and do submit are not allowed together')
    quit()

error = module = script_name = script_path = ''
buffer = script_spec = None

script_location = path.normpath(path.join(__file__, '..', '..', 'src', f'y{year}'))
script_options = [x for x in listdir(script_location) if x.startswith(f'{str(day).zfill(2)}.') and x.endswith('.py')]
if not script_options:
    error = 'There is no script for this day yet.'
else:
    script_name = script_options[0]
    script_path = path.join(script_location, script_name)
    script_spec = spec_from_file_location('script', script_path)
    module = module_from_spec(script_spec)

data_location = path.normpath(path.join(__file__, '..', '..', 'data', f'y{year}', f'{str(day).zfill(2)}.txt'))

if not interactive and not path.exists(data_location):
    python_location = path.normpath(path.join(__file__, '..', '..', 'venv', 'bin', 'python'))
    downloader_location = path.normpath(path.join(__file__, '..', 'download.py'))
    subprocess.call([python_location, downloader_location, str(day), str(year)])

data = stdin.read() if interactive else open(data_location, 'r').read()

while data.startswith('\n'):
    data = data[1:]

while data.endswith('\n'):
    data = data[:-1]

first_message = f'=== Advent of Code december {adjectivized_number(day)} {year} ==='
title = script_name[4:-3]
typical_length = len(first_message)
print(first_message)
if error:
    print(f'! {error} !')
    quit()
print(message_align(title, typical_length))

sys.stdin = StringIO(data)
start = time()

if not direct:
    buffer = StringIO()
    sys.stdout = buffer
    script_spec.loader.exec_module(module)
    sys.stdout = sys.__stdout__
else:
    script_spec.loader.exec_module(module)
end = time()

sys.stdin = sys.__stdin__

if not direct:
    value = buffer.getvalue()
    if '\n\n' in value:
        part1, part2, *_ = value.split('\n\n')
    elif '\n' in value:
        part1, part2, *_ = value.split('\n')
    else:
        part1, part2 = value, ''
    part1 = part1.strip('\n')
    part2 = part2.strip('\n')
    print(message_align('Part 1', typical_length))
    for line in part1.split('\n'):
        print(message_align(line, typical_length, align_character=' ', side_character='|'))
    print(message_align('Part 2', typical_length))
    for line in part2.split('\n'):
        print(message_align(line, typical_length, align_character=' ', side_character='|'))

    if do_submit:
        last = part2 if part2 else part1
        if not last or not isinstance(last, str) or last in ["None", "False"]:
            print(message_align('! Not going to submit falsy answer !', typical_length, align_character=' ', side_character='|'))
        else:
            env_file_location = path.normpath(path.join(__file__, '..', '..', '.env'))
            env_data = {line.split('=')[0]: line.split('=')[1] for line in open(env_file_location, 'r').readlines() if
                        not line.startswith('#')}

            if not env_data.get('SESSION'):
                print('Mising session in .env file!')
                quit()

            submit_url = f"https://adventofcode.com/{year}/day/{day}/answer"
            print(message_align(f'Submitting {last}!', typical_length))
            response = requests.post(
                url=submit_url,
                cookies={"session": env_data['SESSION']},
                headers={"User-Agent": "advent-of-code-data v{}".format('1.2.1')},
                data={"level": 2 if part2 else 1, "answer": last},
            )
            response.raise_for_status()
            message = response.text
            if "That's the right answer" in message:
                webbrowser.open(f"https://adventofcode.com/{year}/day/{day}#part2")
            if "That's not the right answer" in message:
                print(message)
    if copy_to_pasteboard and which('pbcopy'):
        last = part2 if part2 else part1
        subprocess.run("pbcopy", text=True, input=last)
        print(message_align(f'Copied part {2 if part2 else 1} to clip board!', typical_length))

    print(message_align(f'Runtime {human_time(end-start)}', typical_length))

print(message_align('', typical_length, align_character='=', spacing=0))
