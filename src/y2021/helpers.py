import re
from time import time


def neighbours(x, y):
    return (x, y - 1), (x + 1, y), (x, y + 1), (x - 1, y)


def parse_from_re(parser_re, parser_map=None, lines=()):
    parser_map = {} if parser_map is None else parser_map
    for line in lines:
        if match := re.match(parser_re, line):
            r = match.groupdict()
            for k, v in r.items():
                r[k] = parser_map.get(k, str)(v)
            yield r


def human_time(t):
    t *= 1e6
    suffix = 0
    while t > 1000 and suffix < 2:
        t /= 1000
        suffix += 1
    ts = str(float(int(t * 10)) / 10)
    return ts + ['µs', 'ms', 's'][suffix]


def timed(f):
    def with_time(*args, **kwargs):
        s = time()
        r = f(*args, **kwargs)
        duration = time() - s
        return f'{r} ({human_time(duration)})'
    return with_time
