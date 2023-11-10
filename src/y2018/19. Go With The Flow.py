from lib import *
from sys import stdin

program = stdin.read()


def run_program(instructions, register, bound_register=0, max_steps=inf):
    pointer = 0

    while 0 <= pointer < len(instructions) and max_steps:
        register[bound_register] = pointer

        (*code, ca, cb), a, b, c = instructions[pointer]
        code = ''.join(code)
        op = None

        if ca != 'i' and (code != 'se' or cb == 'r'):
            a = register[a]

        if cb != 'i':
            b = register[b]

        if code == 'ad':
            op = add
        elif code == 'mu':
            op = mul
        elif code == 'ba':
            op = iand
        elif code == 'bo':
            op = ior
        elif code == 'se':
            op = uncurry(itemgetter(0))
        elif code == 'gt':
            op = gt 
        elif code == 'eq':
            op = eq

        register[c] = op(a, b)
        pointer = register[bound_register]
        pointer += 1
        max_steps -= 1


register = [0, 0, 0, 0, 0, 0]
instructions = [[line.split(' ')[0], *ints(line)] for line in program.split('\n')]
bound_register = instructions.pop(0)[1]
run_program(instructions, register, bound_register)
print(register[0])
register = [1, 0, 0, 0, 0, 0]
run_program(instructions, register, bound_register, max_steps=100)
target = register[5]
print(sum(n + (target // n if target != n else 0) for n in range(1, int(sqrt(target))+1) if target % n == 0))
