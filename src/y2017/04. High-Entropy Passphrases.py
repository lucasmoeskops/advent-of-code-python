from collections import Counter
from sys import stdin

lines = stdin.read().split('\n')
print(sum(1 for line in lines if Counter(line.split(' ')).most_common(1)[0][1] == 1))
print(sum(1 for line in lines if Counter(map(''.join, map(sorted, line.split(' ')))).most_common(1)[0][1] == 1))
