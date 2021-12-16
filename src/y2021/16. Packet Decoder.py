#!/usr/bin/env python3

"""
AoC Day 16 - Packet Decoder - in Python.
"""

__author__ = "Lucas Moeskops"
__date__ = "2021-12-16"

from functools import partial
from itertools import islice
from math import prod
from sys import stdin

from helpers import timed

raw_packet = stdin.read()

OPERATOR = [sum, prod, min, max, None, int.__gt__, int.__lt__, int.__eq__]


def parse_raw_packet():
    packet = bin(int(raw_packet, 16))[2:]
    if len(packet) % 4:
        packet = packet.zfill((len(packet) + 4) // 4 * 4)
    return packet


def read(it, length):
    return int(''.join(islice(it, length)), 2)


def calculate_version_sum(packet, subpackets=None):
    packet_it = iter(packet)
    version_sum = 0
    read_it = partial(read, packet_it)
    while True and (subpackets is None or (subpackets := subpackets - 1) >= 0):
        try:
            version_sum += read_it(3)
            type_id = read_it(3)
            if type_id == 4:
                while read_it(5) > 0xf:
                    pass
            else:
                length_id = read_it(1)
                if length_id == 0:
                    length = read_it(15)
                    version_sum += calculate_version_sum(list(islice(packet_it, length)))
                elif length_id == 1:
                    version_sum += calculate_version_sum(packet_it, read_it(11))
        except (StopIteration, ValueError):
            break
    return version_sum


def parse_packet(packet, subpackets=None):
    data = []
    packet_it = iter(packet)
    read_it = partial(read, packet_it)
    while True and (subpackets is None or (subpackets := subpackets - 1) >= 0):
        try:
            _, type_id = read_it(3), read_it(3)
            if type_id == 4:
                subdata = []
                while group := list(islice(packet_it, 5)):
                    if len(group) < 5:
                        break
                    subdata.append(''.join(group[1:]))
                    if group[0] == '0':
                        data.append(int(''.join(subdata), 2))
                        break
            else:
                length_id = read_it(1)
                if length_id == 0:
                    d = parse_packet(list(islice(packet_it, read_it(15))))
                elif length_id == 1:
                    d = parse_packet(packet_it, read_it(11))
                data.append(OPERATOR[type_id](*[d] if type_id < 4 else d))
        except (StopIteration, ValueError):
            break
    return data


@timed
def task_1():
    return calculate_version_sum(parse_raw_packet())


@timed
def task_2():
    return parse_packet(parse_raw_packet())[0]


print(f'[Part 1]: {task_1()}')
print(f'[Part 2]: {task_2()}')