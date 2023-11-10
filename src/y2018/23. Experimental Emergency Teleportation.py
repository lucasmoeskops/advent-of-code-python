from lib import *
from sys import stdin


def in_range(x, y, z):
    t = 0
    for i, (*p, r) in enumerate(nanobots):
        if manhattan3d(x, y, z, *p) <= r:
            t += 1
    return t


def total_distance(x, y, z):
    return sum(max(0, manhattan3d(x, y, z, *p) - r)**2 for *p, r in nanobots)


def closest(x, y, z):
    bx, by, bz, br = min(nanobots, key=lambda b: manhattan3d(x, y, z, *b[:3]) - b[4])
    return abs(bx - x), abs(by - y), abs(bz - z), br


def bring_closer(x, y, z, ox, oy, oz, orad):
    remaining = orad
    for c1, c2 in zip((x, y, z), (ox, oy, oz)):
        reserved = orad // 3
        if abs(c1 - c2) <= reserved:
            remaining -= abs(c1 - c2)

        if c1 > c2:
            remaining -= c1 - c2
        elif c1 < c2:
            remaining -= c2 - c1


nanobots = [*map(ints, lines(stdin.read()))]
*strongest, radius = max(nanobots, key=itemgetter(3))
print(sum(1 for *p, r in nanobots if manhattan3d(*strongest, *p) <= radius))
shared_points = defaultdict(int)
for i, (x, y, z, r) in enumerate(nanobots):
    for ox, oy, oz, orad in nanobots:
        if manhattan3d(x, y, z, ox, oy, oz) <= r + orad:
            shared_points[i] += 1
counted = Counter(shared_points.values())
limit = counted.most_common()[0][1]
relevant = [i for i, v in shared_points.items() if v >= limit]
central_point = reduce(add, (Vector(*nanobots[i][:3]) for i in relevant)) / len(relevant)
#
#
#
# print(len(relevant), in_range(*central_point))
# start = set(strongest)
# position = list(strongest)
# for i in range(2000):
#     step = 2**20
#     while step:
#         td = total_distance(*position)
#         t = in_range(*position)
#         for i in range(3):
#             position[i] -= step
#             ltd = total_distance(*position)
#             position[i] += step * 2
#             htd = total_distance(*position)
#             if ltd > td < htd:
#                 ...
#             elif ltd > htd:
#                 position[i] += step
#             elif htd > ltd:
#                 position[i] -= step
#         step //= 2
#     print(td, t, position, step)
# best = 0
# best_at = 0
# scale1 = 1
# scale2 = 0
# scale = 250000
# s1 = [51899998, 47476610, 24900000]
# s2 = [2, -2, -5]
# # for i in count(0):
# #     for x in range(0, i+1):
# #         for y in range(0, i + 1):
# #             for z in range(0, i + 1):
# #                 if x == -i or x == i or y == -i or y == i or z == -i or z == i:
# #                     if (t := in_range(scale * x + s1[0] * scale1 + s2[0] * scale2, scale * y + s1[1] * scale1 + s2[1] * scale2, scale * z + s1[2] * scale1 + s2[2] * scale2)) > best:
# #                         best = t
# #                         best_at = [scale * x + s1[0] * scale1 + s2[0] * scale2, scale * y + s1[1] * scale1 + s2[1] * scale2, scale * z + s1[2] * scale1 + s2[2] * scale2]
# #                         print(best, best_at)
# # quit()
# best = 918
# best_at = [51899998, 47476106, 24900000]
# for s in range(7, -1, -1):
#     d = 10**s
#     for i in [1, 0, 2]:
#         while in_range(*best_at) == best:
#             best_at[i] -= d
#         best_at[i] += d
# print(best_at)
# print(best, manhattan3d(0, 0, 0, *best_at), in_range(*best_at))
# # print(in_range(63011110, 58587720, 36011110))