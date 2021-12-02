from time import time


def human_time(t):
    t *= 1e6
    suffix = 0
    while t > 1000 and (suffix := suffix + 1) < 2:
        t /= 1000
    ts = str(float(int(t * 10)) / 10)
    return ts + ['µs', 'ms', 's'][suffix]


def timed(f):
    def with_time(*args, **kwargs):
        s = time()
        r = f(*args, **kwargs)
        duration = time() - s
        return f'{r} ({human_time(duration)})'
    return with_time
