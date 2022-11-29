from collections import deque
from sys import stdin


STEPS = int(stdin.read())

buffer = deque([0])

for i in range(1, 2018):
    buffer.rotate(-STEPS)
    buffer.appendleft(i)
    buffer.rotate(-1)

print(buffer[0])


# Algorithm by https://www.reddit.com/user/p_tseng/
# Speed up from ~ 5 seconds to 2 milliseconds
value = 0
position = 0
steps_with_append = STEPS + 1
i = 0
while i < 50000000:
    fits = (i - position) // STEPS
    i += fits + 1
    position = (position + (fits + 1) * (STEPS + 1) - 1) % i + 1
    if position == 1:
        value = i

print(value)
