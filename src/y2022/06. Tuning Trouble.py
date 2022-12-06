from sys import stdin

DATA = stdin.read()

for required in (4, 14):
    last_seen = [-required] * 26
    start = 0
    for i in range(len(DATA)):
        if i - start == required:
            print(i)
            break

        c = ord(DATA[i]) - 97

        if last_seen[c] >= start:
            start = last_seen[c] + 1

        last_seen[c] = i
