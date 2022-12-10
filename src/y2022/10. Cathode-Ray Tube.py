from sys import stdin

DATA = stdin.read().strip()

instructions = DATA.split('\n')
cycle = 0
drawing = ""
X = 1
signal_strength_sums = 0
measurements = 0

for instruction in instructions:
    if instruction == 'noop':
        cycles = 1
        amount = 0
    else:
        cycles = 2
        amount = int(instruction.split(' ')[1])

    for cycle in range(cycle, cycle + cycles):
        cycle %= 40

        if not cycle:
            drawing += '\n'

        if not cycle - 19:
            signal_strength_sums += X * (cycle + 1 + 40 * measurements)
            measurements += 1

        drawing += '#' if X - 1 <= cycle <= X + 1 else '.'

    X += amount
    cycle += 1


print(signal_strength_sums)
print()
print(drawing)
