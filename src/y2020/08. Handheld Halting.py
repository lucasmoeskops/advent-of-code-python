from sys import stdin

def run(prog):
    seen = set()
    a, n = (0, 0)
    lib = {'acc': acc, 'jmp': jmp, 'nop': nop}
    while n not in seen and 0 <= n < len(prog):
        seen.add(n)
        op, arg = prog[n]
        a, n = lib[op](a, n, int(arg))
    return a, (n < 0 or n >= len(prog))

def fix(prog):
    for i, (op, arg) in enumerate(prog):
        if op not in ('jmp', 'nop'):
            continue
        new_op = 'nop' if op == 'jmp' else 'jmp'
        new_prog = prog[:]
        new_prog[i] = (new_op, arg)
        out, term = run(new_prog)
        if term:
            return out

acc = lambda a, n, x: (a + x, n + 1)
jmp = lambda a, n, x: (a, n + x)
nop = lambda a, n, x: (a, n + 1)

lines = stdin.read().split('\n')
prog = [tuple(line.split(' ')) for line in lines]

print(f'1: {run(prog)[0]}')
print(f'2: {fix(prog)}')
