from sys import stdin

data = stdin.read()
lines = [[int(value) for value in line.split('\t') if value] for line in data.split('\n') if line]
total = sum(max(line) - min(line) for line in lines)
print(total)

total = sum(x // y for line in lines for i, x in enumerate(line) for j, y in enumerate(line) if x > y and not x % y or x == y and i < j)
print(total)
