from sys import stdin

DATA = stdin.read().split('\n')

rounds = [line.split(' ') for line in DATA]
rounds = [("ABC".index(a), "XYZ".index(b)) for a, b in rounds]

print(sum(1 + me + 3 * ((1 + me - opponent) % 3) for opponent, me in rounds))
print(sum(1 + me * 3 + (2 + me + opponent) % 3 for opponent, me in rounds))
