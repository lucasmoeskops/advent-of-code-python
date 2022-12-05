from sys import stdin

RUCKSACKS = stdin.read().split('\n')


def priority(item):
    if item >= 'a':
        return ord(item) - ord('a') + 1
    return ord(item) - ord('A') + 27


def items_in_both_compartments():
    for rucksack in RUCKSACKS:
        half = len(rucksack) // 2
        half_1_unique = set(rucksack[half:])
        half_2 = rucksack[:half]

        for item in half_1_unique:
            if item in half_2:
                yield item


def badges():
    for i in range(0, len(RUCKSACKS), 3):
        rucksack_1_unique = set(RUCKSACKS[i])
        rucksack_2, rucksack_3 = RUCKSACKS[i + 1], RUCKSACKS[i + 2]

        for item in rucksack_1_unique:
            if item in rucksack_2 and item in rucksack_3:
                yield item


print(sum(map(priority, items_in_both_compartments())))
print(sum(map(priority, badges())))
