"""
AoC Day 16 - Flawed Frequency Transmission
"""

__author__ = "Lucas Moeskops"
__date__ = "2023-11-06"
__summary__ = "Clean up signals received from earth"

from lib import *
from sys import stdin


def phase(signal, repeat=1, skip=0, skipped=0):
    def range_sum(start, end):
        start = min(start, total_length)
        end = min(end, total_length)

        if start == end:
            return 0

        if repeat == 1:
            return prefix_sum[end] - prefix_sum[start]

        initial = start % len(signal), len(signal) if end >= (start // len(signal) + 1) * len(signal) else end % len(signal)

        num_rounds = end // len(signal) - start // len(signal)
        if num_rounds == 0:
            return prefix_sum[initial[1]] - prefix_sum[initial[0]]
        else:
            final = 0, end % len(signal)
            return prefix_sum[initial[1]] - prefix_sum[initial[0]] + prefix_sum[final[1]] - prefix_sum[final[0]] + (num_rounds - 1) * total_sum

    prefix_sum = [0, *accumulate(signal)]
    total_sum = prefix_sum[-1]
    total_length = len(signal) * repeat

    return [
        abs(sum(
            range_sum(j, j+i) - range_sum(j+i*2, j+i*3)
            for j in range(i-skipped-1, total_length, i*4)
        )) % 10
        for i in range(skip+skipped+1, total_length+skipped+1)
    ]


puzzle_raw = stdin.read()
puzzle = list(map(int, puzzle_raw))
skip = int(puzzle_raw[:7])
print(''.join(str(x) for x in nth(transform_stream(phase, puzzle), 100)[:8]))
x = phase(puzzle, repeat=10000, skip=skip)
print(''.join(str(x) for x in nth(transform_stream(partial(phase, skipped=skip), x), 99)[:8]))
