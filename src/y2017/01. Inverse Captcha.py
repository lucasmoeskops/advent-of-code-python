from sys import stdin

data = stdin.read()
total = sum(int(data[i]) for i in range(len(data)) if data[i] == data[(i+1) % len(data)])
print(total)

half = len(data) // 2
total = sum(int(data[i]) for i in range(len(data)) if data[i] == data[(i+half) % len(data)])
print(total)
