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
version_sum = 0
message = None

OPERATOR = [sum, prod, min, max, None, int.__gt__, int.__lt__, int.__eq__]


def parse_raw_packet():
    packet = bin(int(raw_packet, 16))[2:]
    if len(packet) % 4:
        packet = packet.zfill((len(packet) + 4) // 4 * 4)
    return packet


def read(it, length):
    return int(''.join(islice(it, length)), 2)


def parse_packet(packet, sub_packets=None):
    global version_sum
    data, packet_it = [], iter(packet)
    read_it = partial(read, packet_it)
    while sub_packets is None or (sub_packets := sub_packets - 1) >= 0:
        try:
            version_sum += read_it(3)
            type_id = read_it(3)
            if type_id == 4:
                subdata = []
                while group := list(islice(packet_it, 5)):
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
        except ValueError:
            break
    return data


@timed
def task_1():
    if version_sum == 0:
        parse_packet(parse_raw_packet())
    return version_sum


@timed
def task_2():
    global message
    if message is None:
        message = ''.join(map(str, parse_packet(parse_raw_packet())))
    return message


print(f'[Part 1]: {task_1()}')
print(f'[Part 2]: {task_2()}')