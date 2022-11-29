from itertools import count
from operator import itemgetter
from sys import stdin


DESCRIPTIONS = stdin.read().split('\n')

layers = {int(description.split(':')[0]): int(description.split(': ')[1]) for description in DESCRIPTIONS}
print(sum(layer * depth for layer, depth in layers.items() if not layer % ((depth - 1) * 2)))

sorted_layers = sorted(layers.items(), key=itemgetter(1))
timed_layers = [(layer, ((depth - 1) * 2)) for layer, depth in sorted_layers]

for n in count():
    for layer, timed in timed_layers:
        if not (n + layer) % timed:
            break
    else:
        break

print(n)
