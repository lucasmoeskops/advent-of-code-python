from collections import defaultdict
from sys import stdin

data = stdin.read()
stack = []
reading_garbage = False
ignore_next = False
score = 0
garbage_count = 0
for c in data:
    if reading_garbage:
        if ignore_next:
            ignore_next = False
        elif c == '>':
            reading_garbage = False
        elif c == '!':
            ignore_next = True
        else:
            garbage_count += 1
        continue
    if c == '{':
        stack.append(None)
    elif c == '}':
        score += len(stack)
        stack.pop()
    elif c == '<':
        reading_garbage = True
print(score)
print(garbage_count)
