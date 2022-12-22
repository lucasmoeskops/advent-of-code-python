from collections import defaultdict
from sys import stdin

DATA = stdin.read()

TOP = 0
RIGHT = 1
BOTTOM = 2
LEFT = 3


def move(monkey_map, position, direction, amount):
    dx = 1 if direction == RIGHT else -1 if direction == LEFT else 0
    dy = 1 if direction == BOTTOM else -1 if direction == TOP else 0
    x, y = position

    while amount:
        x += dx
        y += dy

        if monkey_map[(x, y)] == '#':
            return x - dx, y - dy, direction

        if monkey_map[(x, y)] == ' ':
            nx = x - dx
            ny = y - dy

            while monkey_map[(nx, ny)] != ' ':
                nx -= dx * 50
                ny -= dy * 50

            if monkey_map[(nx + dx, ny + dy)] == '#':
                return x - dx, y - dy, direction

            x, y = nx, ny
        else:
            amount -= 1
    return x, y, direction


def rotate(towards, direction):
    return (direction + 1 if towards == 'R' else direction - 1) % 4


def move_cube(monkey_map, position, direction, amount):
    dx = 1 if direction == RIGHT else -1 if direction == LEFT else 0
    dy = 1 if direction == BOTTOM else -1 if direction == TOP else 0
    x, y = position

    while amount:
        x += dx
        y += dy

        if monkey_map[(x, y)] == '#':
            return x - dx, y - dy, direction

        if monkey_map[(x, y)] == ' ':
            if 50 <= x < 100 and y == -1 and direction == TOP:
                # TOP BACK FACING BACKWARDS
                nx, ny = 0, x + 100
                new_direction = RIGHT
            elif 100 <= x < 150 and y == -1 and direction == TOP:
                # RIGHT BACK FACING BACKWARDS
                nx, ny = x - 100, 199
                new_direction = TOP
            elif x == 49 and 0 <= y < 50 and direction == LEFT:
                # TOP LEFT FACING LEFT
                nx, ny = 0, -y + 149
                new_direction = RIGHT
            elif x == 150 and 0 <= y < 50 and direction == RIGHT:
                # RIGHT BOTTOM FACING DOWN
                nx, ny = 99, -y + 149
                new_direction = LEFT
            elif 100 <= x < 150 and y == 50 and direction == BOTTOM:
                # RIGHT FRONT FACING TOWARDS YOU
                nx, ny = 99, x - 50
                new_direction = LEFT
            elif x == 49 and 50 <= y < 100 and direction == LEFT:
                # FRONT LEFT FACING LEFT
                nx, ny = y - 50, 100
                new_direction = BOTTOM
            elif x == 100 and 50 <= y < 100 and direction == RIGHT:
                # FRONT RIGHT FACING RIGHT
                nx, ny = y + 50, 49
                new_direction = TOP
            elif 0 <= x < 50 and y == 99 and direction == TOP:
                # LEFT FRONT FACING TOWARDS YOU
                nx, ny = 50, x + 50
                new_direction = RIGHT
            elif x == 100 and 100 <= y < 150 and direction == RIGHT:
                # BOTTOM RIGHT FACING RIGHT
                nx, ny = 149, -(y - 100) + 49
                new_direction = LEFT
            elif 50 <= x < 100 and y == 150 and direction == BOTTOM:
                # BOTTOM BACK FACING BACKWARDS
                nx, ny = 49, x + 100
                new_direction = LEFT
            elif x == -1 and 100 <= y < 150 and direction == LEFT:
                # LEFT TOP FACING TOPWARDS
                nx, ny = 50, -y + 149
                new_direction = RIGHT
            elif x == -1 and 150 <= y < 200 and direction == LEFT:
                # BACK TOP FACING TOPWARDS
                nx, ny = y - 100, 0
                new_direction = BOTTOM
            elif x == 50 and 150 <= y < 200 and direction == RIGHT:
                # BACK BOTTOM FACING DOWNWARDS
                nx, ny = y - 100, 149
                new_direction = TOP
            elif 0 <= x < 50 and y == 200 and direction == BOTTOM:
                # BACK RIGHT FACING RIGHT
                nx, ny = x + 100, 0
                new_direction = BOTTOM
            else:
                print('Dont understand', x, y, dx, dy)
                quit()

            if monkey_map[(nx, ny)] == '#':
                return x - dx, y - dy, direction

            direction = new_direction
            dx = 1 if direction == RIGHT else -1 if direction == LEFT else 0
            dy = 1 if direction == BOTTOM else -1 if direction == TOP else 0
            x, y = nx, ny

        amount -= 1
    return x, y, direction


def do():
    map_raw, notes = DATA.split('\n\n')
    monkey_map = defaultdict(lambda: ' ')

    for y, row in enumerate(map_raw.split('\n')):
        for x, val in enumerate(row):
            monkey_map[(x, y)] = val

    for variant in (move, move_cube):
        number = None
        position = DATA.index('.'), 0
        direction = RIGHT

        for c in notes:
            if '0' <= c <= '9':
                if number is None:
                    number = int(c)
                else:
                    number = 10 * number + int(c)
            else:
                *position, direction = variant(monkey_map, position, direction, number)
                direction = rotate(c, direction)
                number = None

        if number is not None:
            *position, direction = variant(monkey_map, position, direction, number)

        x, y = position
        print((y + 1) * 1000 + 4 * (x + 1) + (direction + 3) % 4)


do()
