from sys import stdin

DATA = stdin.read().split('\n\n')

elf_calories = [sum(map(int, elf.split('\n'))) for elf in DATA]
elf_calories = sorted(elf_calories, reverse=True)

print(elf_calories[0])
print(sum(elf_calories[:3]))
