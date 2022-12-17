from collections import defaultdict
from dataclasses import dataclass
from itertools import cycle, count
from math import gcd
from sys import stdin

DATA = stdin.read().strip()

SHAPES = """
####

.#.
###
.#.

..#
..#
###

#
#
#
#

##
##
""".strip()

WIDTH = 7
LEFT = -1
RIGHT = 1


@dataclass
class Rock:
    shape: str
    width: int
    height: int

    x: int = 2
    y: int = 0


def rock_from_shape(shape):
    width = shape.index('\n') + 1
    return Rock(shape, width, len(shape) // width)


def push(map_, rock: Rock, direction):
    for y in range(rock.height):
        for x in range(rock.width):
            if rock.shape[y * rock.width + x] == '#':
                if not (0 <= rock.x + x + direction < 7) or map_[(x + rock.x + direction, rock.y - y)] != '.':
                    return False

    rock.x += direction
    return True


def fall(map_, rock: Rock):
    for y in range(rock.height):
        for x in range(rock.width):
            if rock.shape[y * rock.width + x] == '#':
                if map_[(x + rock.x, rock.y - y - 1)] != '.':
                    return False

    rock.y -= 1
    return True


def place(map_, rock: Rock):
    for y in range(rock.height):
        for x in range(rock.width):
            if rock.shape[y * rock.width + x] == '#':
                map_[(x + rock.x, rock.y - y)] = '#'
    return True


def map_hash(map_, height):
    t = []
    for i in range(7):
        h = 0
        while map_[(i, height - h)] == '.':
            h += 1
        t.append(h)
    return tuple(t)


class HeightGiver:
    def __init__(self):
        self.brute_forced = [0]
        self.cycle_start = None
        self.cycle_length = None
        self.cycle_height = None
        self.shapes = cycle((shape + '\n') for shape in SHAPES.split('\n\n'))
        self.jet_pattern = cycle(DATA)
        self.map_ = defaultdict(lambda: '.')
        num_shapes = len(SHAPES.split('\n\n'))
        num_patterns = len(DATA)
        self.lcm = num_patterns * num_shapes // gcd(num_patterns, num_shapes)

        for rocks_stopped in range(7):
            self.map_[(rocks_stopped, 0)] = '-'

    def find_height(self, num_rocks):
        if self.cycle_start is None:
            self.find_pattern()

        if num_rocks < self.cycle_start:
            return self.brute_forced[num_rocks]

        amount, left_over = divmod(num_rocks - self.cycle_start, self.cycle_length)
        return amount * self.cycle_height + self.brute_forced[self.cycle_start + left_over]

    def find_pattern(self):
        lookup, map_, shape_generator, jet_pattern_generator, lcm = (
            self.brute_forced, self.map_, self.shapes, self.jet_pattern, self.lcm
        )
        height = 0
        patterns_seen = {}
        lcm_index = 0

        for rocks_stopped in count(1):
            cycle_pattern = lcm_index % lcm, map_hash(map_, height)

            if cycle_pattern in patterns_seen:
                height_then, then = patterns_seen[cycle_pattern]
                self.cycle_start = then
                self.cycle_length = rocks_stopped - then
                self.cycle_height = height - height_then
                break

            patterns_seen[cycle_pattern] = (height, rocks_stopped)

            shape = next(shape_generator)
            rock = rock_from_shape(shape)
            rock.y = start = height + rock.height + 4

            while fall(map_, rock):
                jet_push = next(jet_pattern_generator)
                push(map_, rock, LEFT if jet_push == '<' else RIGHT)

            place(map_, rock)
            lcm_index += start - rock.y
            height = max(height, rock.y)
            lookup.append(height)


height_giver = HeightGiver()
print('{}\n{}'.format(*(height_giver.find_height(n) for n in (2022, 1000000000000))))
