from collections import deque, defaultdict
from functools import partial
from sys import stdin


INSTRUCTIONS = [line.split(' ') for line in stdin.read().split('\n')]


def resolve(state, value):
    if '0' <= value[0] <= '9' or value[0] == '-':
        return int(value)
    return state['registers'][value]


def run(condition=lambda x: False, initial=None, state_extra=None, state=None):
    state = state or {
        'registers': defaultdict(int, initial if initial else []),
        'pointer': 0,
        'out': deque(),
        'in': deque(),
        **(state_extra or {}),
    }
    _resolve = partial(resolve, state)
    while not condition(state):
        name, *values = INSTRUCTIONS[state['pointer']]
        match name:
            case 'snd':
                state['out'].append(_resolve(values[0]))
            case 'set':
                state['registers'][values[0]] = _resolve(values[1])
            case 'add':
                state['registers'][values[0]] += _resolve(values[1])
            case 'mul':
                state['registers'][values[0]] *= _resolve(values[1])
            case 'mod':
                state['registers'][values[0]] %= _resolve(values[1])
            case 'rcv':
                if state['in']:
                    state['registers'][values[0]] = state['in'].popleft()
                else:
                    break
            case 'jgz':
                if _resolve(values[0]) > 0:
                    state['pointer'] += _resolve(values[1]) - 1
        state['pointer'] += 1

    return state


print(run(lambda state: state['in'])['out'][-1])


class CountingDeque(deque):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.append_counter = 0

    def append(self, x):
        self.append_counter += 1
        super().append(x)


message_queue_0 = CountingDeque()
message_queue_1 = deque()
program_0 = run(initial={'p': 0}, state_extra={'in': message_queue_0, 'out': message_queue_1})
program_1 = run(initial={'p': 1}, state_extra={'in': message_queue_1, 'out': message_queue_0})
while message_queue_0 or message_queue_1:
    program_0 = run(state=program_0)
    program_1 = run(state=program_1)
print(message_queue_0.append_counter)
