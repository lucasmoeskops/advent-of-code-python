from sys import stdin

lines = stdin.read().split('\n')
instructions = [int(line) for line in lines]

pointer = 0
steps = 0
while 0 <= pointer < len(instructions):
    new_pointer = pointer + instructions[pointer]
    instructions[pointer] += 1
    pointer = new_pointer
    steps += 1
print(steps)

instructions = [int(line) for line in lines]
pointer = 0
steps = 0
while 0 <= pointer < len(instructions):
    new_pointer = pointer + instructions[pointer]
    if instructions[pointer] >= 3:
        instructions[pointer] -= 1
    else:
        instructions[pointer] += 1
    pointer = new_pointer
    steps += 1
print(steps)
