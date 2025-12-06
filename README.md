# Advent of Code

Solutions to Advent of Code problems. The code for the solutions is adjusted after submitting it for the challenge to how I think it is most readable or fastest or preferably both.

## How to run

### Setup an env

```python3 -m venv venv --prompt="aoc"```

### Add your session cookie value to the .env file:

E.g.

```SESSION=123f1231a2312b3121e3123d12f```

### Add scripts to execute

Scripts go in `src/y<year>/<%2day>. Name.py`, e.g. `src/y2017/01. Inverse Captcha.py`.

### Run script

#### Default

To run day 1 of 2017:

```./scripts/run.py 1 2017```

### Interactive

To run day 1 of 2017 and give input in the command line:

```./scripts/run.py 1 2017 -i```

Paste or type input and press <enter>, then <Ctrl> + d.

### Showing all printed output instead of trying to parse part1 and part2 outputs:

```./scripts/run.py 1 2017 -d```

### Submitting answer to Advent of Code

```./scripts/run.py 1 2017 -s```

### Haskell

[Work in progress]

```runghc path/to/file.hs < path/to/input.txt```

## Runtimes

### Year 2024

|    Day  | Title                                |        Runtime        | ✓/✗  |
| ------- |--------------------------------------| --------------------- | --- |
|  Day  1 | Historian Hysteria                   |   9.2 milliseconds    | ✓   |
|  Day  2 | Red-Nosed Reports                    |   1.8 milliseconds    | ✓   |
|  Day  3 | Mull It Over                         |   652 microseconds    | ✓   |
|  Day  4 | Ceres Search                         |   1.7 milliseconds    | ✓   |
|  Day  5 | Print Queue                          |   1.5 milliseconds    | ✓   |
|  Day  6 | Guard Gallivant                      |   757 milliseconds    | ✓   |
|  Day  7 | Bridge Repair                        |   880 milliseconds    | ✓   |
|  Day  8 | Resonant Collinearity                |   935 microseconds    | ✓   |
|  Day  9 | Disk Fragmenter                      |    19 milliseconds    | ✓   |
|  Day 10 | Hoof It                              |   3.8 milliseconds    | ✓   |
|  Day 11 | Plutonian Pebbles                    |    48 milliseconds    | ✓   |
|  Day 12 | Garden Groups                        |    19 milliseconds    | ✓   |
|  Day 13 | Claw Contraption                     |   4.6 milliseconds    | ✓   |
|  Day 14 | Restroom Redoubt                     |    36 milliseconds    | ✓   |
|  Day 15 | Warehouse Woes                       |    17 milliseconds    | ✓   |
|  Day 16 | Reindeer Maze                        |   579 milliseconds    | ✓   |
|  Day 17 | Chronospatial Computer               |   1.0 milliseconds    | ✓   |
|  Day 18 | RAM Run                              |    11 milliseconds    | ✓   |
|  Day 19 | Linen Layout                         |   338 milliseconds    | ✓   |
|  Day 20 | Race Condition                       |        2.5 seconds    | ✓   |
|  Day 21 | Keypad Conundrum                     |    10 milliseconds    | ✓   |
|  Day 22 | Monkey Market                        |        1.4 seconds    | ✓   |
|  Day 23 | LAN Party                            |    25 milliseconds    | ✓   |
|  Day 24 | Crossed Wires                        |   2.2 milliseconds    | ✓   |
|  Day 25 | Code Chronicle                       |    21 milliseconds    | ✓   |
| ------- | ------------------------------------ | --------------------- | --- |
|   |     Total runtime:                   |                                         6.8 seconds |

### Year 2025

|    Day  | Title                                |        Runtime        | ✓/✗  |
| ------- |--------------------------------------| --------------------- | --- |
|  Day  1 | Secret Entrance                      |   5.7 milliseconds    | ✓   |
|  Day  2 | Gift Shop                            |   402 microseconds    | ✓   |
|  Day  3 | Lobby                                |   2.0 milliseconds    | ✓   |
|  Day  4 | Printing Department                  |    34 milliseconds    | ✓   |
|  Day  5 | Cafetaria                            |   551 microseconds    | ✓   |
|  Day  6 | Trash Compactor                      |   1.5 milliseconds    | ✓   |
| ------- | ------------------------------------ | --------------------- | --- |
||  Total runtime:                      |      45 milliseconds |     |
