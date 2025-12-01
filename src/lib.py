from termcolor import colored

oldpow = pow
from typing import *
from bisect import *
from collections import *
from copy import copy, deepcopy
from enum import Enum
from functools import *
from heapq import *
from itertools import *
from math import *
from operator import *
from string import *
from time import *

pow = oldpow

import re
MAYBE_LATER = 2

UP = 0
RIGHT = 1
DOWN = 2
LEFT = 3


def read_input(year, day, strip=True):
    with open(f'data/y{year}/{str(day).zfill(2)}.txt') as f:
        return f.read().strip() if strip else f.read()


def ints(s):
    return [int(v) for v in re.findall(r'(-?\d+)\D', s + '*')]


def tokens(s, skip=' ', transform=None):
    at = 0
    tks = []
    for i, c in enumerate(s):
        if c in skip:
            if at < i:
                new_token = s[at:i]
                if transform is not None and len(transform) > len(tks) and callable(transform[len(tks)]):
                    new_token = transform[len(tks)](new_token)
                tks.append(new_token)
            at = i + 1
    if at < len(s):
        new_token = s[at:len(s)]
        if transform is not None and len(transform) > len(tks):
            new_token = transform[len(tks)](new_token)
        tks.append(new_token)
    return tks


def lines(s, transform=None):
    if transform is None:
        return s.split('\n')
    return [transform(line) for line in s.split('\n')]


def intlines(s):
    return [ints(line) for line in lines(s)]


def unique(iterable):
    out = []
    seen = set()
    for i in iterable:
        if i not in seen:
            out.append(i)
            seen.add(i)
    return out


def clamp(mn, mx, value):
    return max(mn, min(mx, value))


def chunks(iterable, length=None, amount=None):
    if length is None:
        if not isinstance(iterable, Sequence):
            iterable = list(iterable)

        length, extra = divmod(len(iterable), amount)
        at = 0
        for i in range(amount):
            new_at = at + length + (extra > 0)
            yield iterable[at:new_at]
            at = new_at
            if extra:
                extra -= 1
        return

    if isinstance(iterable, str):
        stitch = ''.join
    else:
        stitch = list

    if not isinstance(iterable, Iterator):
        iterable = iter(iterable)

    while True:
        chunk = stitch(islice(iterable, length))
        if chunk:
            yield chunk
        else:
            break


def debug(x, hard=False, key=lambda x: x):
    print(key(x))
    if hard:
        import pdb;
        pdb.set_trace()
    return x


def triangle_sum(b, a=0):
    return (a + b) * (b - a + 1) // 2


def grid2d(s='', default='.', skip=''):
    grid = defaultdict(lambda: default)
    for y, row in enumerate(lines(s)):
        for x, c in enumerate(row):
            if c not in skip and c != default:
                grid[x, y] = c
    return grid


def range2d(sx, sy, ex, ey):
    return list(product(
        range(sx, ex, 1 if ex >= sx else -1),
        range(sy, ey, 1 if ey >= sy else -1),
    ))


def range3d(sx, sy, sz, ex, ey, ez):
    return list(product(
        range(sx, ex, 1 if ex >= sx else -1),
        range(sy, ey, 1 if ey >= sy else -1),
        range(sz, ez, 1 if ez >= sz else -1)
    ))


def move2d(x, y, direction, n=1) -> Tuple[int, int]:
    if direction == UP:
        return x, y - n
    if direction == RIGHT:
        return x + n, y
    if direction == DOWN:
        return x, y + n
    return x - n, y  # LEFT


def delta2d4(n=1):
    return [(0, -n), (n, 0), (0, n), (-n, 0)]


def delta2d8(n=1):
    return [(0, -n), (n, -n), (n, 0), (n, n), (0, n), (-n, n), (-n, 0), (-n, -n)]


def neighbors2d4(x, y, n=1):
    return [(x, y - n), (x + n, y), (x, y + n), (x - n, y)]


def neighbours2d6(x, y):
    return [(x + 1, y - 1), (x + 2, y), (x + 1, y + 1), (x - 1, y + 1), (x - 2, y), (x - 1, y - 1)]


def neighbors2d8(x, y):
    return [(x, y - 1), (x + 1, y - 1), (x + 1, y), (x + 1, y + 1), (x, y + 1), (x - 1, y + 1), (x - 1, y),
            (x - 1, y - 1)]


def neighborsnd(*positions, dimensions=3):
    for delta in product(*(range(-1, 2) for _ in range(dimensions))):
        if all(v == 0 for v in delta):
            continue
        yield tuple(a + b for a, b in zip(positions, delta))


def rotate2d(x, y, d):
    d = d % 4
    if d == 0:
        return x, y
    elif d == 1:
        return y, -x
    elif d == 2:
        return -x, -y
    elif d == 3:
        return -y, x


def raytracer2d(grid, can_skip=None, fallback='.', borders=None):
    can_skip = (lambda p: p not in grid) if can_skip is None else can_skip
    borders = borders2d(grid) if borders is None else borders

    def trace(x, y, dx, dy, skip_function=None):
        if skip_function is None:
            skip_function = can_skip

        while True:
            x, y = x + dx, y + dy
            if not within2d(*borders, x, y):
                return fallback
            if not skip_function((x, y)):
                return grid[x, y]

    return trace


def floodfill(grid, x, y, before, after):
    if grid[x, y] != before:
        return 0

    grid[x, y] = after
    score = 1
    for nx, ny in neighbors2d4(x, y):
        if grid[nx, ny] == before:
            score += floodfill(grid, nx, ny, before, after)
    return score


def borders2d(coords, one_bigger_for_range=True):
    if not isinstance(coords, Sequence):
        coords = list(coords)
    return min(map(itemgetter(0), coords)), \
           min(map(itemgetter(1), coords)), \
           max(map(itemgetter(0), coords)) + one_bigger_for_range, \
           max(map(itemgetter(1), coords)) + one_bigger_for_range


def bordersnd(coords, dimensions=3):
    for dimension in range(dimensions):
        yield min(map(itemgetter(dimension), coords))
    for dimension in range(dimensions):
        yield max(map(itemgetter(dimension), coords)) + 1


def mul2d(n, x, y):
    return x * n, y * n


def add2d(x, y, dx, dy):
    return x + dx, y + dy


def neg2d(x, y):
    return -x, -y


def within2d(min_x, min_y, max_x, max_y, x, y, allow_on_border=True):
    if allow_on_border:
        return min_x <= x < max_x and min_y <= y < max_y
    return min_x < x < max_x - 1 and min_y < y < max_y - 1


def manhattan2d(sx, sy, ex, ey):
    return abs(sx - ex) + abs(sy - ey)


def manhattan3d(sx, sy, sz, ex, ey, ez):
    return abs(sx - ex) + abs(sy - ey) + abs(sz - ez)


def reading_order(coords):
    return sorted(coords, key=itemgetter(1, 0))


def normalize_grid(grid, default='.'):
    redundant = [p for p, c in grid.items() if c == default]
    for key in redundant:
        del grid[key]
    return grid


def grid_hash(grid, default='.'):
    return tuple(normalize_grid(grid).items())


def print2d(coords):
    grid = {(x, y) for x, y, *_ in coords}
    min_x, min_y, max_x, max_y = borders2d(coords)
    for y in range(min_y, max_y):
        line = ''
        for x in range(min_x, max_x):
            line += '#' if (x, y) in grid else '.'
        print(line)


def printgrid(grid, min_x=0, min_y=0, max_x=None, max_y=None, size=None, default=' '):
    if size is not None and max_x is None:
        max_x = min_x + size
    if size is not None and max_y is None:
        max_y = min_y + size
    if size is None:
        min_x, min_y, max_x, max_y = borders2d(grid)
    for y in range(min_y, max_y):
        line = ""
        for x in range(min_x, max_x):
            line += grid.get((x, y), default)
        print(line.rstrip())


def rotate_matrix(m):
    return list(zip(*m))[::-1]


def flip_matrix(m):
    return [row[::-1] for row in m]


def fliprotate_variants(m):
    for i in range(4):
        yield m
        m = rotate_matrix(m)
    m = flip_matrix(m)
    for i in range(4):
        yield m
        m = rotate_matrix(m)


def grid_to_matrix(grid):
    min_x, min_y, max_x, max_y = borders2d(grid)
    matrix = []
    for y in range(min_y, max_y):
        row = []
        for x in range(min_x, max_x):
            row.append(grid[x, y])
        matrix.append(row)
    return matrix


def matrix_to_grid(matrix):
    grid = grid2d()
    for y, row in enumerate(matrix):
        for x, value in enumerate(row):
            grid[x, y] = value
    return grid


def bfs_raw(start=(), get_options=lambda o: (), evaluate=lambda s: False, key=lambda x, steps: x, max_steps=inf,
            visualize=False):
    queue = deque([(0, None, o) for o in start])
    seen = {key(o, 0): 0 for o in start}
    solutions = []
    curmax = None

    while queue:
        steps, parent, o = queue.popleft()

        if steps <= max_steps and evaluate(o):
            parent, path = parent, [o]

            while parent:
                path.append(parent.val)
                parent = parent.next

            solutions.append(path[::-1][1:])
            max_steps = steps

        if steps >= max_steps:
            continue

        opts = list(get_options(o))
        for option in opts:
            if (k := key(option, steps + 1)) not in seen:
                seen[k] = steps + 1
                queue.append((steps + 1, Node(o, parent), option))

    if visualize:
        printgrid({**(visualize if isinstance(visualize, dict) else {}), **{k: '*' for k in seen},
                   **{key(o): 'O' for path in solutions for o in path}, **{key(o): 'S' for o in start},
                   **{key(path[-1]): 'X' for path in solutions}}, size=None)


    # max_steps = steps

    return solutions, seen


def bfs(start=(), get_options=lambda o: (), evaluate=lambda s: False, key=lambda x, s: x, max_steps=inf,
        visualize=False):
    return bfs_raw(start, get_options, evaluate, key, max_steps, visualize)[0]


def distances(start=(), get_options=lambda o: (), evaluate=lambda s: False, key=lambda x, s: x, max_steps=inf,
              visualize=False):
    return bfs_raw(start, get_options, evaluate, key, max_steps, visualize)


def astar(start: List[Any] = (), get_options: Callable[Any, Tuple[int, Any]] = lambda o: (),
          evaluate: Callable[Any, bool] = lambda s: False, h: Callable[Any, float] = lambda x: 1,
          key: Callable[Any, Any] = lambda x: x, visualize=False):
    heap = [(h(o), 0, None, o) for o in start]

    seen = defaultdict(lambda: inf)

    while heap:
        _, steps, parent, o = heappop(heap)

        if evaluate(o):
            path = [(steps, o)]
            while parent:
                path.append(parent.val)
                parent = parent.next
            if visualize:
                to_pnt = itemgetter(0, 1)
                printgrid({**(visualize if isinstance(visualize, dict) else {}),
                           **{to_pnt(k): colored(visualize[to_pnt(k)], 'blue') for k in seen}, **{
                        to_pnt(key(o)): colored('^>V<'[neighbors2d4(*to_pnt(key(o))).index(to_pnt(key(p)))], 'yellow')
                        for (_, p), (_, o) in pairwise(path)}, **{to_pnt(key(o)): colored('S', 'green') for o in start},
                           to_pnt(key(path[0][1])): colored('X', 'yellow')}, size=None, default='.')
            return steps, path[::-1]

        for cost, option in get_options(o):
            total = steps + cost
            if seen[k := key(option)] > total:
                seen[k] = total
                heappush(heap, (total + h(option), total, Node((steps, o), parent), option))

    if visualize:
        printgrid({**(visualize if isinstance(visualize, dict) else {}), **{k: '*' for k in seen},
                   **{key(o): 'S' for o in start}}, size=None)


def backtrack(nodes, options_for_node=lambda assignment, node: [], satisfies=lambda assignment: True,
              option_hash=lambda x: x):
    def backtrack_inner(assignment, available_nodes):
        if not available_nodes:
            if satisfies(assignment):
                return assignment
            return

        options = {}
        for node in available_nodes:
            if (node_options := options_for_node(assignment, node)) is not None:
                options[node] = node_options
            else:
                return

        option_count = Counter(map(
            option_hash,
            chain.from_iterable(
                []
                if options[node] == MAYBE_LATER
                else options[node]
                for node in available_nodes
            )
        ))

        best_node = min(available_nodes, key=lambda node: inf if options[node] == MAYBE_LATER else len(options[node]))
        option_order = sorted(
            options[best_node],
            key=lambda option: option_count[option_hash(option)],
            reverse=True)
        available_nodes.remove(best_node)

        for option in option_order:
            assignment_copy = assignment.copy()
            assignment_copy[best_node] = option

            if result := backtrack_inner(assignment_copy, available_nodes):
                return result

        available_nodes.add(best_node)

    return backtrack_inner({}, set(nodes))


def curry(f):
    return lambda args: f(*args)


def uncurry(f):
    return lambda *args: f(args)


def negate(f):
    return lambda *args: not f(*args)


def apply(*fs):
    return lambda *args: (f(a) for f, a in zip(fs, args))


def select(f, *items):
    ig = itemgetter(*items)
    return (lambda *args: f(ig(args))) if len(items) == 1 else (lambda *args: f(*ig(args)))


def nth(stream, n, default=None):
    return next(islice(stream, n, n + 1)) if default is None else next(islice(stream, n, n + 1), default)


def starfilter(f, iterable):
    return filter(lambda x: f(*x), iterable)


def first_duplicate(stream, key=lambda x: x):
    seen = set()
    for v in stream:
        k = key(v)
        if k in seen:
            return v
        seen.add(k)


def period(stream, key=lambda x: x):
    seen = {}
    for i, v in enumerate(stream):
        k = key(v)
        if k in seen:
            # start, length of period
            return seen[k], i - seen[k]
        seen[k] = i


def item_of_periodic(stream, idx, key=lambda x: x):
    i, j = tee(stream)
    s, l = period(i, key=key)
    if idx < s:
        return nth(j, idx)
    return nth(j, (idx - s) % l + s)


def make_graph(nodes=(), connections=(), bidirectional=True):
    graph = Graph()
    for node in nodes:
        graph.add_node(node)
    for a, b in connections:
        graph.add_edge(a, b, bidirectional=bidirectional)
    return graph


def transform_stream(transform, state):
    while True:
        yield state
        state = transform(state)


def until(transform, state, evaluator=lambda old, new: old == new):
    old_state = state
    state = transform(state)
    runs = 0
    while not evaluator(old_state, state):
        old_state = state
        try:
            state = transform(state)
        except KeyboardInterrupt:
            print(state)
            raise
        runs += 1
    return old_state, runs


def kcluster(k, points, distance=manhattan2d):
    clusters = defaultdict(list)
    centers = [points[i] for i in range(k)]
    while True:
        for point in points:
            clusters[centers.index(min(centers, key=lambda center: distance(*point, *center)))].append(point)
        new_centers = [tuple(int(0.5 + sum(c) / len(cluster)) for c in zip(*cluster)) for cluster in clusters.values()]
        if centers == new_centers:
            return clusters, centers
        centers = new_centers
        clusters = defaultdict(list)


def make_sumgrid(sx, sy, ex, ey, value_at=lambda x, y: 0):
    grid = defaultdict(int)
    for x, y in range2d(sx, sy, ex, ey):
        grid[x, y] = value_at(x, y) + grid[x, y - 1] + grid[x - 1, y] - grid[x - 1, y - 1]
    return grid


def print_tree(tree, key=str, max_width=None):
    def print_tree_inner(tree):
        line_groups = []
        for child in tree.children:
            child_lines = print_tree_inner(child)
            if child_lines:
                width = max(map(len, child_lines)) + 1
                child_lines = [line + (width - len(line)) * ' ' for line in child_lines]
            else:
                width = 1
                child_lines = ['']
            if line_groups:
                if len(child_lines) > len(line_groups[-1]):
                    for line_group in line_groups:
                        width_ = len(line_group[-1])
                        for i in range(len(line_group), len(child_lines)):
                            line_group.append(' ' * width_)
                for i in range(len(child_lines), len(line_groups[-1])):
                    child_lines.append(' ' * width)
            line_groups.append(child_lines)
        lines = [key(tree.val) or '?']
        if len(line_groups) > 1:
            second = '├'
            second += '-' * max(0, (len(line_groups[0][0]) - 1))
            for line_group in line_groups[1:-1]:
                second += '┬' + '-' * max(0, len(line_group[0]) - 1)
            second += '┐'
            lines.append(second)
            lines.extend(map(''.join, zip_longest(*line_groups, fillvalue=' ')))
        elif len(line_groups) == 1:
            lines.extend(line_groups[0])
        return lines

    if max_width is not None:
        print('\n'.join(s[:max_width] for s in print_tree_inner(tree)))
    else:
        print('\n'.join(print_tree_inner(tree)))


def kahn(graph):
    in_degree = {node: 0 for node in graph}
    for node, children in graph.items():
        for child in children:
            in_degree[child] += 1

    queue = deque(node for node in graph if in_degree[node] == 0)
    order = []

    while queue:
        node = queue.popleft()
        order.append(node)

        for child in graph[node]:
            in_degree[child] -= 1
            if in_degree[child] == 0:
                queue.append(child)

    return order


class Edge2:
    def __init__(self, id, weight=1, from_node=None, to_node=None, graph=None):
        self.id = id
        self.weight = weight
        self.from_node_id = from_node
        self.to_node_id = to_node
        self._graph = graph

    @property
    def from_node(self):
        return self._graph._make_node(self.from_node_id)

    @property
    def to_node(self):
        return self._graph._make_node(self.to_node_id)


class Node2:
    def __init__(self, id, value, graph):
        self.id = id
        self.value = value
        self._graph = graph

    def add_edge(self, other_node_id, weight=1):
        return self._graph.add_edge(self.id, other_node_id, weight=weight)

    def edges(self):
        return self._graph.edges(self.id)

    def bfs(self, other_node_id):
        return self._graph.bfs(self.id, other_node_id)

    def bfs_negative(self, other_node_id):
        return self._graph.bfs_negative(self.id, other_node_id)

    def dfs_longest(self, other_node_id):
        return self._graph.dfs_longest(self.id, other_node_id)


class Graph2:
    def __init__(self):
        self._node_id_inc = 1
        self._edge_id_inc = 1
        self._edges = defaultdict(list)
        self._edge_weights = defaultdict(lambda: 1)
        self._edge_nodes = defaultdict(list)
        self._node_values = {}

    def add_node(self, value):
        new_id = self._node_id_inc
        self._node_id_inc += 1
        self._node_values[new_id] = value
        return self._make_node(new_id)

    def add_edge(self, from_node_id, to_node_id, weight=1):
        if isinstance(from_node_id, Node2):
            from_node_id = from_node_id.id
        if isinstance(to_node_id, Node2):
            to_node_id = to_node_id.id
        new_id = self._edge_id_inc
        self._edge_id_inc += 1
        self._edge_nodes[new_id].append(from_node_id)
        self._edge_nodes[new_id].append(to_node_id)
        self._edges[from_node_id].append(new_id)
        self._edges[to_node_id].append(new_id)
        self._edge_weights[new_id] = weight
        return self._make_edge(new_id, from_node_id)

    def edges(self, node_id):
        if isinstance(node_id, Node2):
            node_id = node_id.id
        return [self._make_edge(edge_id, node_id) for edge_id in self._edges[node_id]]

    def bfs_negative(self, start_node_id, end_node_id):
        if isinstance(start_node_id, Node2):
            start_node_id = start_node_id.id
        if isinstance(end_node_id, Node2):
            end_node_id = end_node_id.id

        queue = deque([(0, start_node_id, {start_node_id})])
        total_weight = sum(self._edge_weights.values())
        heap = [(total_weight, 0, start_node_id, {start_node_id})]
        best = 0

        while queue:
            max_possible, steps, node_id, seen = heappop(heap)

            if node_id == end_node_id:
                best = min(best, steps)
                print(best, max_possible)
                if best >= max_possible:
                    break

            edges = self._make_node(node_id).edges()
            max_possible -= sum(edge.weight for edge in edges)

            for edge in edges:
                if edge.to_node_id not in seen:
                    # seen.add(edge.to_node_id)
                    heappush(heap, (max_possible + edge.weight, steps - edge.weight, edge.to_node_id, seen | {edge.to_node_id}))

        return -best

    def dfs_longest(self, start_node_id, end_node_id):
        if isinstance(start_node_id, Node2):
            start_node_id = start_node_id.id
        if isinstance(end_node_id, Node2):
            end_node_id = end_node_id.id
        longest = -1

        queue = [(0, start_node_id, {start_node_id})]

        while queue:
            steps, node_id, seen = queue.pop()

            if node_id == end_node_id:
                if steps > longest:
                    longest = steps

            for edge in self._make_node(node_id).edges():
                if edge.to_node_id not in seen:
                    queue.append((steps + edge.weight, edge.to_node_id, seen | {edge.to_node_id}))

        return longest

    def _make_edge(self, edge_id, from_node_id):
        to_node = [n for n in self._edge_nodes[edge_id] if n != from_node_id][0]
        return Edge2(edge_id, weight=self._edge_weights[edge_id], from_node=from_node_id, to_node=to_node, graph=self)

    def _make_node(self, node_id):
        return Node2(node_id, value=self._node_values[node_id], graph=self)


class Graph:
    def __init__(self):
        self.values = []
        self.edges = defaultdict(list)
        self.edgeNames = defaultdict(dict)
        self.weights = defaultdict(int)

    def __getitem__(self, item):
        return self.values.__getitem__(item)

    def __setitem__(self, item, value):
        return self.values.__setitem__(item, value)

    def __delitem__(self, item):
        self.values[item] = None
        if item in self.edges:
            del self.edges[item]
            for node in self.edges:
                if item in self.edges[node]:
                    print(True)
                    self.edges[node].remove(item)
                    if not self.edges[node]:
                        del self.edges[node]
        if item in self.edgeNames:
            del self.edgeNames[item]

    def add_node(self, value):
        self.values.append(value)
        return len(self.values) - 1

    def add_edge(self, i, j, weight=1, name=None, bidirectional=True, reverse_name=None):
        if isinstance(i, str):
            if i not in self.values:
                i = self.add_node(i)
            else:
                i = self.values.index(i)
        if isinstance(j, str):
            if j not in self.values:
                j = self.add_node(j)
            else:
                j = self.values.index(j)
        self.edges[i].append(j)
        self.edges
        if name:
            self.edgeNames[i][name] = j
        if bidirectional:
            self.edges[j].append(i)
            if reverse_name:
                self.edgeNames[j][reverse_name] = i

    def has_edge(self, i, j):
        return j in self.edges[i]

    def get_edge_by_name(self, node, name):
        return self.edgeNames[node].get(name)

    def get_name_by_edge(self, node, other_node):
        return next((name for name, x in self.edgeNames[node].items() if x == other_node), None)

    def get_edges(self, node):
        if isinstance(node, str):
            node = self.values.index(node)
        return self.edges[node]

    def get_edges_as_values(self, node):
        if isinstance(node, str):
            node = self.values.index(node)
        return [self.values[k] for k in self.edges[node]]


class Vector:
    def __init__(self, *values):
        self.values = list(values)

    def __add__(self, other):
        if isinstance(other, Vector):
            return Vector(*map(add, self.values, other.values))

        return Vector(*map(add, self.values, repeat(other)))

    def add(self, other):
        if isinstance(other, Vector):
            for i, j in zip(range(len(self.values)), other.values):
                self.values[i] += j
            return self

        for i in range(len(self.values)):
            self.values[i] += other

        return self

    def __mul__(self, other):
        if isinstance(other, Vector):
            return Vector(*map(mul, self.values, other.values))

        return Vector(*map(mul, self.values, repeat(other)))

    def __sub__(self, other):
        if isinstance(other, Vector):
            return Vector(*map(sub, self.values, other.values))

        return Vector(*map(sub, self.values, repeat(other)))

    def __truediv__(self, other):
        if isinstance(other, Vector):
            return Vector(*map(truediv, self.values, other.values))

        return Vector(*map(truediv, self.values, repeat(other)))

    def __floordiv__(self, other):
        if isinstance(other, Vector):
            return Vector(*map(floordiv, self.values, other.values))

        return Vector(*map(floordiv, self.values, repeat(other)))

    def __mod__(self, other):
        if isinstance(other, Vector):
            return Vector(*map(mod, self.values, other.values))

        return Vector(*map(mod, self.values, repeat(other)))

    def __pow__(self, other):
        if isinstance(other, Vector):
            return Vector(*map(pow, self.values, other.values))

        return Vector(*map(pow, self.values, repeat(other)))

    def __neg__(self):
        return Vector(*map(neg, self.values))

    def __abs__(self):
        return Vector(*map(abs, self.values))

    def __eq__(self, other):
        if isinstance(other, Vector):
            return self.values == other.values

        return self.values == other

    def __repr__(self):
        return f"Vector({', '.join(map(str, self.values))})"

    def __getitem__(self, item):
        return self.values[item]

    def __setitem__(self, key, value):
        self.values[key] = value

    def __len__(self):
        return len(self.values)

    def __iter__(self):
        return iter(self.values)

    def __hash__(self):
        return tuple(self.values).__hash__()

    def __copy__(self):
        return Vector(*self.values)

    def index(self, item):
        return self.values.index(item)

    def manhattan(self, other):
        return sum(abs(a - b) for a, b in zip(self.values, other.values))

    def neighbors(self, n):
        for c in range(len(self.values)):
            v = copy(self)
            v[c] += n
            yield v
            v = copy(self)
            v[c] -= n
            yield v

    def sign(self):
        v = copy(self)
        for i in range(len(v.values)):
            v[i] = 1 if v[i] > 0 else -1 if v[i] < 0 else 0
        return v


class NamedVector(Vector):
    names = ()
    values = None

    def __init__(self, *values):
        self.names = {}
        self.values = []
        for i, value in enumerate(values):
            if isinstance(value, str):
                self.names[value] = i
                self.values.append(i)
            else:
                self.values.append(value)

    def __getattribute__(self, item):
        if item not in ['names'] and item in self.names:
            return self.values[self.names[item]]

        return super().__getattribute__(item)

    def __setattr__(self, item, value):
        if item not in ['names'] and item in self.names:
            self.values[self.names[item]] = value

        super().__setattr__(item, value)


class Rational:
    def __init__(self, nominator, denominator):
        print('from', nominator, denominator, end=' ')
        if isinstance(nominator, Rational):
            denominator *= nominator.denominator
            nominator = nominator.nominator
        print('(', nominator, denominator, ')', end=' ')

        if isinstance(denominator, Rational):
            nominator *= denominator.denominator
            denominator = denominator.nominator
        print('to', nominator, denominator)

        gcd_ = gcd(nominator, denominator)
        self.nominator = nominator // gcd_
        self.denominator = denominator // gcd_

    def __add__(self, other):
        if isinstance(other, Rational):
            return Rational(
                self.denominator * other.nominator + self.nominator * other.denominator,
                self.denominator * other.denominator
            )

        return Rational(self.nominator + self.denominator * other, self.denominator)

    def __mul__(self, other):
        if isinstance(other, Rational):
            return Rational(
                self.nominator * other.nominator,
                self.denominator * other.denominator
            )

        return Rational(self.nominator * other, self.denominator)

    def __str__(self):
        return f'{self.nominator}/{self.denominator}'

    def __repr__(self):
        return f'{self.nominator}/{self.denominator}'


class Node:
    def __init__(self, val=None, next=None):
        self.val = val
        self.next = next

    def __lt__(self, other):
        return id(self) < id(other)


class DoubleLinkedNode:
    def __init__(self, val=None, prev=None, next=None):
        self.val = val
        self.prev = prev
        self.next = next


class TreeNode:
    def __init__(self, val=None, children=None, parent=None):
        self.val = val
        self.children = []
        self.parent = parent
        if children:
            for child in children:
                self.add_child(child)

    def add_child(self, val):
        if isinstance(val, TreeNode):
            val.parent = self
            child = val
        else:
            child = TreeNode(val=val, parent=self)
        self.children.append(child)
        return child

    def __repr__(self):
        return self.val

    def __str__(self):
        return self.val
