from sys import stdin

PAIRS = stdin.read().split('\n')

comparisons = [tuple(map(int, pair.replace('-', ',').split(','))) for pair in PAIRS]
print(sum(1 for a, b, c, d in comparisons if a <= c and b >= d or c <= a and d >= b))
print(sum(1 for a, b, c, d in comparisons if b >= c and a <= d or c >= b and d <= a))
