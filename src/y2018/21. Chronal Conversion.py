from lib import *
from sys import stdin

program = stdin.read()


def run_program(instructions, register, bound_register=0, max_steps=inf):
    global first, last
    pointer = 0

    while 0 <= pointer < len(instructions) and max_steps:
        register[bound_register] = pointer

        if pointer == 18:
            register[1] = register[2] // 256

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

        if pointer == 28:
            if not seen:
                first = register[3]

            if register[3] not in seen:
                seen.add(register[3])
                last = register[3]

        pointer = register[bound_register]
        pointer += 1
        max_steps -= 1


register = [0, 0, 0, 0, 0, 0]
instructions = [[line.split(' ')[0], *ints(line)] for line in program.split('\n')]
bound_register = instructions.pop(0)[1]
seen = set()
first = None
last = None
# TODO: figure out why 1000000 is sufficient
run_program(instructions, register, bound_register, max_steps=1000000)
print(first)
print(last)
