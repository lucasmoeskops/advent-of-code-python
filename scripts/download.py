#!/usr/bin/env python
from datetime import date
from os import path
import requests
from sys import argv

URL = "https://adventofcode.com/{year}/day/{day}/input"

env_file_location = path.normpath(path.join(__file__, '..', '..', '.env'))
env_data = {line.split('=')[0]: line.split('=')[1].strip() for line in open(env_file_location, 'r').readlines() if not line.startswith('#')}

if not env_data.get('SESSION'):
    print('Mising session in .env file!')
    quit()

flags = [x[1:] for x in argv if x.startswith('-')]
rest = [x for x in argv if not x.startswith('-')]

day = int(rest[1]) if len(rest) > 1 else date.today().day
year = int(rest[2]) % 100 if len(rest) > 2 else date.today().year % 100
year += 2000

data_location = path.normpath(path.join(__file__, '..', '..', 'data', f'y{year}', f'{str(day).zfill(2)}.txt'))
if path.exists(data_location):
    print('Data is already downloaded and located at ', data_location)
    quit()

data = requests.get(URL.format(year=year, day=day), cookies={"session": env_data['SESSION']}, headers={"User-Agent": "advent-of-code-data v{}".format('1.2.1')})
data.raise_for_status()
open(data_location, 'wb').write(data.content)
