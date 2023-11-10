from lib import *
from sys import stdin

description = stdin.read()
grid = grid2d('', default='#')

root = node = TreeNode((0, 0))
stack = [[[root], []]]
nodes = [node]
endpoints = stack[-1][1]
grid[0, 0] = 'X'

for i, c in enumerate(description):
    if c in 'NESW':
        seen = set()
        new_nodes = []
        for node in nodes:
            new_door = move2d(*node.val, 'NESW'.index(c))
            grid[new_door] = '-' if c in 'NS' else '|'
            new_position = move2d(*node.val, 'NESW'.index(c), n=2)
            grid[new_position] = '.'
            if new_position not in seen:
                seen.add(new_position)
                new_nodes.append(node.add_child(new_position))
        nodes = new_nodes
    elif c == '|':
        endpoints.extend(nodes)
        nodes = stack[-1][0]
    elif c == '(':
        stack.append([nodes, []])
        endpoints = stack[-1][1]
    elif c == ')':
        new_nodes = []
        seen = set()
        for node in chain(nodes, endpoints):
            if node not in seen:
                seen.add(node)
                new_nodes.append(node)
        nodes = new_nodes
        stack.pop()
        endpoints = stack[-1][1]


def get_options(position):
    for door, room in zip(neighbors2d4(*position), neighbors2d4(*position, 2)):
        if grid[door] != '#':
            yield room


room_distances = distances([(0, 0)], get_options)
print(max(room_distances.values()))
print(ceil(sum(1 for (x, y), v in room_distances.items() if v >= 1000)))
