#!/usr/bin/env python3

"""
AoC Day 2 - Gift Shop - in Python.
"""

__author__ = "Lucas Moeskops"
__date__ = "2025-12-02"
__summary__ = "Gift Shop"

from itertools import count

from lib import read_input

puzzle = read_input(2025, 2)

# Parse ranges and sort them
ranges = puzzle.split(',')
ranges = [range_str.split('-') for range_str in ranges]
ranges = [[int(start), int(end)] for start, end in ranges]
ranges.sort()

# Store candidates for both parts
for_part_1 = set()
for_part_2 = set()

# Keep track of which range we are processing
range_pointer = 0

# We will check ranges by powers of ten which will be contained in range_start to range_end
range_end = 0

# Check in increasing powers of ten
for power in count(1):
    # Stop if all ranges are processed
    if range_pointer >= len(ranges):
        break

    # Define the current range
    range_start = range_end + 1
    range_end = 10**power - 1

    # Process all ranges that intersect with the current range
    while range_pointer < len(ranges) and ranges[range_pointer][0] <= range_end:
        # Determine the range within the current power of ten range
        start = max(range_start, ranges[range_pointer][0])
        end = min(range_end * 10, ranges[range_pointer][1])

        # Check for each possible amount of digits in the repeating part
        for amount in range(1, power + 1):
            # Integer step size for the current amount of digits
            step_size = 10**amount

            # Take the first part of the number
            part = start * step_size // (range_end + 1)

            # Test if it is repeating at most twice for part 1
            twice = step_size**2 > range_start

            while part < step_size:
                # Build the next candidate by repeating the part until it comes within the range start
                candidate = part * step_size + part
                while candidate < range_start:
                    candidate = candidate * step_size + part

                # Count it if it is within the range
                if start <= candidate <= end:
                    (for_part_1 if twice else for_part_2).add(candidate)
                elif candidate > end:
                    break

                # Try the next part
                part += 1

        # Move to the next range if we are done with the current one
        if ranges[range_pointer][1] <= range_end:
            range_pointer += 1
        else:
            # Move to the next power and keep the same range
            break

print(sum(for_part_1))  # Part 1
print(sum(for_part_1 | for_part_2))  # Part 2
