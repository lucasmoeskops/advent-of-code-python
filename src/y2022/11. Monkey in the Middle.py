from collections import deque
from functools import partial
from typing import NamedTuple, Sequence, Callable, Tuple

from math import prod
from sys import stdin

DATA = stdin.read()


class MonkeyDefinition(NamedTuple):
    items: Tuple
    increase_worry: Callable[[int], int]
    test: int
    on_true: int
    on_false: int


class Monkey(NamedTuple):
    definition: MonkeyDefinition
    items: deque
    inspections: Sequence[int]


def get_monkeys() -> Tuple[Monkey]:
    return tuple(
        Monkey(definition, deque(definition.items), [0])
        for definition in monkey_definitions
    )


def make_worry_increaser(operator, amount) -> Callable[[int], int]:
    if operator == '*':
        if amount == 'old':
            return lambda item: item * item
        return partial(int.__mul__, int(amount))
    if amount == 'old':
        return partial(int.__mul__, 2)
    return partial(int.__add__, int(amount))


def round(monkeys: Sequence[Monkey], decrease_worry: bool = True) -> None:
    for monkey in monkeys:
        definition, items, inspections = monkey
        _, increase_worry, divider, on_true, on_false = definition

        while items:
            item = items.popleft()
            inspections[0] += 1
            item = increase_worry(item) % modulo
            if decrease_worry:
                item //= 3
            throw_to = on_false if item % divider else on_true
            monkeys[throw_to].items.append(item)


monkey_definitions = []

for monkey_raw in DATA.split('\n\n'):
    lines = monkey_raw.split('\n')
    monkey = MonkeyDefinition(
        items=tuple(map(int, lines[1].replace(',', '').split(' ')[4:])),
        increase_worry=make_worry_increaser(*lines[2].rsplit(' ', maxsplit=2)[-2:]),
        test=int(lines[3].split(' ')[-1]),
        on_true=int(lines[4].split(' ')[-1]),
        on_false=int(lines[5].split(' ')[-1]),
    )
    monkey_definitions.append(monkey)

modulo = prod(definition.test for definition in monkey_definitions)


for num_rounds, worry in ((20, True), (10000, False)):
    monkeys = get_monkeys()
    for _ in range(num_rounds):
        round(monkeys, worry)
    print(prod(sorted(monkey.inspections[0] for monkey in monkeys)[-2:]))
