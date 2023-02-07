from dataclasses import dataclass
from math import prod, inf
from sys import stdin

DATA = stdin.read().strip()


@dataclass
class Ingredient:
    capacity: int
    durability: int
    flavor: int
    texture: int
    calories: int


def calc_score(amounts, allow_negative=False, target_calories=None):
    """ Calculate score for given division of ingredients.

    If allow_negative is True, scores containing a negative value will result in a
    negative score instead of 0.

    If target_calories is an integer, divisions that do not result in the target
    amount of calories will be 0.

    If both allow_negative is True and target_calories is an integer, scores will
    be divided by the distance to the target calorie amount.
    """
    capacity = durability = flavor = texture = calories = 0

    for ingredient, amount in zip(ingredients.values(), amounts):
        capacity += amount * ingredient.capacity
        durability += amount * ingredient.durability
        flavor += amount * ingredient.flavor
        texture += amount * ingredient.texture

        if calories is not None:
            calories += amount * ingredient.calories

    if allow_negative:
        has_negative = capacity < 0 or durability < 0 or flavor < 0 or texture < 0
        non_zero = [value for value in (capacity, durability, flavor, texture) if value != 0]
        amount = -abs(prod(non_zero)) if has_negative else prod(non_zero)

        if target_calories is not None:
            return amount / (1 + abs(calories - target_calories))

        return amount

    if capacity <= 0 or durability <= 0 or flavor <= 0 or texture <= 0:
        return 0

    if target_calories is not None and calories != target_calories:
        return 0

    return capacity * durability * flavor * texture


def dfs(teaspoons=100, target_calories=None):
    num_ingredients = len(ingredients)
    initial_division = [teaspoons // num_ingredients] * num_ingredients
    initial_division[-1] += teaspoons % (teaspoons // num_ingredients)
    best = -inf
    queue = [(tuple(initial_division), best)]
    seen = set()

    while queue:
        division, score = queue.pop()
        final_score = calc_score(division, target_calories=target_calories)

        if final_score > best:
            best = final_score

        new = list(division)

        for i in range(num_ingredients):
            new[i] += 1

            for j in range(num_ingredients):
                if i == j:
                    continue

                new[j] -= 1
                new_division = tuple(new)

                if new_division not in seen:
                    seen.add(new_division)
                    new_score = calc_score(new_division, allow_negative=True, target_calories=target_calories)

                    if new_score > score:
                        queue.append((new_division, new_score))

                new[j] += 1

            new[i] -= 1

    return best


ingredients = {}

for line in DATA.split('\n'):
    name, properties = line.split(': ', maxsplit=1)
    kwargs = {}

    for property in properties.split(', '):
        property_name, property_value = property.split(' ', maxsplit=1)
        kwargs[property_name] = int(property_value)

    ingredients[name] = Ingredient(**kwargs)

print(dfs())
print(dfs(target_calories=500))
