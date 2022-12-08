from sys import stdin

DATA = stdin.read()


def scan(start, end, step):
    values = [0]
    visible.add(start)
    for n, i in enumerate(range(start+step, end, step), start=1):
        value = map_[i]
        count = 1
        while len(values) > count and map_[i - count * step] < value:
            count += values[n - count]
        if count == len(values) and value > map_[start]:
            visible.add(i)
        values.append(count)
    return values


size = DATA.index('\n')
map_ = DATA.replace('\n', '')
visible = set()
sight = [1] * size * size

for n in range(size):
    ranges = (
        (n, len(map_), size),
        (len(map_) - size + n, -1, -size),
        (n * size, (n + 1) * size, 1),
        ((n + 1) * size - 1, n * size - 1, -1),
    )
    for range_ in ranges:
        data = iter(scan(*range_))
        for i in range(*range_):
            sight[i] *= next(data)

print(len(visible))
print(max(sight))
