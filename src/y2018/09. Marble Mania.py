from sys import stdin
from collections import defaultdict
from functools import partial, reduce

class Marble:
    def __init__(self, number, previous=None, next=None):
        self.number = number
        self.previous = previous or self
        self.next = next or self

    def travel(self, steps):
        if steps > 0:
            return self.next.travel(steps - 1)
        return self if steps == 0 else self.previous.travel(steps + 1)

    def insert_after(self, number):
        new_marble = self.next.previous = self.next = Marble(number, self, self.next)
        return new_marble

    def remove(self):
        self.previous.next = self.next
        self.next.previous = self.previous
        self.next = self.previous = self
        return self

def new_circle():
    return Marble(0)

def place_marble(num_players, scores, current: Marble, number: int):
    if number % 23:
        return current.travel(1).insert_after(number)
    current = current.travel(-6)
    removed = current.travel(-1).remove()
    scores[number % num_players] += number + removed.number
    return current

def play(num_players, num_marbles):
    scores = defaultdict(int)
    reduce(partial(place_marble, num_players, scores), range(1, num_marbles), new_circle())
    return max(scores.values())

words = stdin.read().split(' ')
num_players, num_marbles = map(int, [words[0], words[6]])

print(f'1: {play(num_players, num_marbles)}')
print(f'2: {play(num_players, num_marbles * 100)}')
