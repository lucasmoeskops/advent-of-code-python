from sys import stdin

RUCKSACKS = stdin.read().split('\n')


def priority(item):
    if item >= 'a':
        return ord(item) - ord('a') + 1
    return ord(item) - ord('A') + 27


def items_in_both_compartments():
    for rucksack in RUCKSACKS:
        half = len(rucksack) // 2
        half_1, half_2 = rucksack[:half], rucksack[half:]

        for item in half_1:
            if item in half_2:
                yield item
                break


def badges():
    for rucksack_a, rucksack_b, rucksack_c in zip(RUCKSACKS[::3], RUCKSACKS[1::3], RUCKSACKS[2::3]):
        for item in rucksack_a:
            if item in rucksack_b and item in rucksack_c:
                yield item
                break


print(sum(map(priority, items_in_both_compartments())))
print(sum(map(priority, badges())))
