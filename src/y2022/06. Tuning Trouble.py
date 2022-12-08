from sys import stdin

DATA = stdin.read()
ASCII_A = ord('a')

for required in (4, 14):
    last_seen = [-1] * 26
    start = 0
    for i in range(len(DATA)):
        if i - start == required:
            print(i)
            break

        c = ord(DATA[i]) - ASCII_A

        if last_seen[c] >= start:
            start = last_seen[c] + 1

        last_seen[c] = i
