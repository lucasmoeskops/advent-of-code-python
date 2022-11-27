from sys import stdin

numbers = stdin.read().split('\t')
blocks = [int(number) for number in numbers]

seen = {tuple(blocks): 0}
while True:
    biggest = max(range(len(blocks)), key=blocks.__getitem__)
    per_block, left_over = divmod(blocks[biggest], len(blocks))
    on_left_side = (biggest + left_over) - len(blocks)
    for i in range(len(blocks)):
        if i == biggest:
            blocks[i] = per_block
        elif i < biggest:
            blocks[i] += per_block + (i <= on_left_side)
        else:
            blocks[i] += per_block + (i - biggest <= left_over)

    if tuple(blocks) in seen:
        break

    seen[tuple(blocks)] = len(seen)
print(len(seen))
print(len(seen)-seen[tuple(blocks)])
