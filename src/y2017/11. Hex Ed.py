from sys import stdin


PATH = stdin.read().split(',')


def calculate_distance(x, y):
    return abs(x) + abs(abs(y) - abs(x)) // 2


def follow_path():
    x = y = most = 0

    for item in PATH:
        if item == 'ne':
            y -= 1
            x += 1
        elif item == 'se':
            y += 1
            x += 1
        elif item == 's':
            y += 2
        elif item == 'sw':
            y += 1
            x -= 1
        elif item == 'nw':
            y -= 1
            x -= 1
        elif item == 'n':
            y -= 2
        most = max(most, calculate_distance(x, y))
    return calculate_distance(x, y), most


distance, furthest = follow_path()

print(distance)
print(furthest)
