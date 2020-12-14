from itertools import product
from re import match
from sys import stdin

def write_mem(mem, mask, address, value):
    b = bin(value)[2:].zfill(36)
    b = [b[i] if m == 'X' else m for i, m in enumerate(mask)]
    mem[address] = int(''.join(b), 2)

def write_mem2(mem, mask, address, value):
    xs = [i for i, m in enumerate(mask) if m == 'X']
    bin_address = bin(address)[2:].zfill(36)
    bin_address = ['1' if m == '1' else bin_address[i] for i, m in enumerate(mask)]
    for p in product('01', repeat=len(xs)):
        write_address = bin_address[:]
        for d, x in zip(p, xs):
            write_address[x] = d
        mem[''.join(write_address)] = value

def run(mem, writer, lines):
    for line in lines:
        m = match(r'mem\[(\d+)\] = (\d+)', line)
        if m:
            writer(mem, mask, *map(int, m.groups()))
        else:
            mask = line.split(' = ')[1]

lines = stdin.read().split('\n')

mem = {}
run(mem, write_mem, lines)

print(f'1: {sum(mem.values())}')

mem = {}
run(mem, write_mem2, lines)

print(f'2: {sum(mem.values())}')