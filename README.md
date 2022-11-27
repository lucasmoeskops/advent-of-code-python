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