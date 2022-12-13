from functools import cmp_to_key
from sys import stdin

DATA = stdin.read()


def cmp(x, y):
    if type(x) != type(y):
        if type(x) == int:
            x = [x]
        else:
            y = [y]

    if type(x) == int:
        return (x > y) - (y > x)

    for a, b in zip(x, y):
        if compare := cmp(a, b):
            return compare

    return cmp(len(x), len(y))


def parse(line):
    stack = []
    skip_close = -1
    for i, c in enumerate(line):
        if '0' <= c <= '9':
            if type(stack[-1]) == int:
                stack[-1] = 10 * stack[-1] + int(c)
            else:
                stack.append(int(c))
        elif c == ',':
            x = stack.pop()
            stack[-1].append(x)
        elif c == ']' and skip_close != i:
            x = stack.pop()
            stack[-1].append(x)
        elif c == '[':
            stack.append([])
            skip_close = i + 1
    return stack[0]


inputs = [parse(line) for line in DATA.split('\n') if line]
print(sum(i // 2 + 1 for i in range(0, len(inputs), 2) if cmp(*inputs[i:i + 2]) == -1))

inputs.extend(([[2]], [[6]]))
sort = sorted(inputs, key=cmp_to_key(cmp))
print((sort.index([[2]]) + 1) * (sort.index([[6]]) + 1))
