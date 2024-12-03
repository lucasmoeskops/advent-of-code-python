from inspect import isgenerator

from lib import *

test_counter = 0

def run_test(answer, test, explanation, *args, **kwargs):
    global test_counter
    test_counter += 1
    start = time()
    result = test(*args, **kwargs)
    end = time()
    duration = end - start
    if isgenerator(result):
        result = list(result)
    icon = '✅' if result == answer else '❌'
    if result != answer:
        error = colored(f' (result: {result})', 'red')
        title = colored(test.__name__, 'red')
    else:
        error = ''
        answer = colored(answer, 'green')
        title = colored(test.__name__, 'green')
    name = f'{title}({", ".join(map(repr, args))}{", " if args and kwargs else ""}{", ".join(f"{k}={repr(v)}" for k, v in kwargs.items())}) == {answer}{error}'
    print(colored(explanation, 'blue'))
    print(f'{icon} {duration:.6f}s {test_counter:02}: {name}')

run_test([1, 0, 4, 5, -600, 8], ints, "Find all integers in a string", 'one1_0,4.5*-600o8')

test = 'aap beer hond'
answer = ['aap', 'beer', 'hond']
explanation = "Tokenize a string"
run_test(answer, tokens, explanation, test)

test = 'aap,beer..hond'
answer = ['aap', 'beer', 'hond']
explanation = "Tokenize a string with multiple custom or duplicated separators"
run_test(answer, tokens, explanation, test, ',.')

test = 'aap,3,hond,4,beer'
answer = ['aap', 3, 'hond', 4, 'beer']
explanation = "Tokenize a string and apply transform[i] on token[i]"
run_test(answer, tokens, explanation, test, ',', [str, int, str, int])

test = 'aap\nbeer\nhond'
answer = ['aap', 'beer', 'hond']
explanation = "Tokenize a string with a newline separator"
run_test(answer, tokens, explanation, test, '\n')

test = 'aap\nbeer\nhond'
answer = ['aap', 'beer', 'hond']
explanation = "Get lines from a string"
run_test(answer, lines, explanation, test)

test = '1\n2\n3'
answer = [1, 2, 3]
explanation = "Get lines from a string and apply a transform on each line"
run_test(answer, lines, explanation, test, int)

test = '1 2 3\n4 5 6\n7 8 9'
answer = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
explanation = "Get lines from a string and search integers on each line"
run_test(answer, intlines, explanation, test)

test = ['aap', 'beer', 'beer', 'hond', 'hond', 'hond']
answer = ['aap', 'beer', 'hond']
explanation = "Remove duplicates from a list"
run_test(answer, unique, explanation, test)

test = 500
answer = 100
explanation = "Limit a number to a range"
run_test(answer, clamp, explanation, 0, 100, test)

test = -100
answer = 0
explanation = "Limit a number to a range"
run_test(answer, clamp, explanation, 0, 100, test)

test = "abcdefghijklmnopqrstuvwxyz"
answer = ["abcd", "efgh", "ijkl", "mnop", "qrst", "uvwx", "yz"]
explanation = "Split a string in parts of a fixed length"
run_test(answer, chunks, explanation, test, 4)

test = [1, 2, 3, 4, 5, 6, 7, 8, 9]
answer = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
explanation = "Split a list in parts of a fixed length"
run_test(answer, chunks, explanation, test, 3)

test = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
answer = [[1, 2, 3, 4], [5, 6, 7], [8, 9, 10]]
explanation = "Split a list in a fixed amount of parts"
run_test(answer, chunks, explanation, test, amount=3)

test = 5
answer = 15
explanation = "Calculate the sum of the first n natural numbers"
run_test(answer, triangle_sum, explanation, test)

test = [5, 3]
answer = 12
explanation = "Calculate the sum of the natural numbers from b to a"
run_test(answer, triangle_sum, explanation, *test)

test = '.#.\n#.#\n.#.'
answer = {(0, 1): '#', (1, 0): '#', (1, 2): '#', (2, 1): '#'}
explanation = "Find the coordinates of all non-dot characters in a grid"
run_test(answer, grid2d, explanation, test)

test = '.#.\n#.#\n.#.'
answer = {(0, 0): '.', (0, 2): '.', (1, 1): '.', (2, 0): '.', (2, 2): '.'}
explanation = "Find the coordinates of certain characters in a grid"
run_test(answer, grid2d, explanation, test, default='#')

test = '.#.\n#.#\n.#.'
answer = {(0, 0): '.', (0, 1): '#', (0, 2): '.', (1, 0): '#', (1, 1): '.', (1, 2): '#', (2, 0): '.', (2, 1): '#', (2, 2): '.'}
explanation = "Find the coordinates of non-space characters in a grid"
run_test(answer, grid2d, explanation, test, default=' ')

test = '.#.\n#.#\n.#.'
answer = {(0, 1): '#', (1, 0): '#', (1, 2): '#', (2, 1): '#'}
explanation = "Find the coordinates of non-space characters in a grid if they are not dots"
run_test(answer, grid2d, explanation, test, default=' ', skip='.')

test = [0, 0, 3, 3]
answer = [(0, 0), (0, 1), (0, 2), (1, 0), (1, 1), (1, 2), (2, 0), (2, 1), (2, 2)]
explanation = "Generate all coordinates in a grid"
run_test(answer, range2d, explanation, *test)

test = [2, 2, -1, -1]
answer = [(2, 2), (2, 1), (2, 0), (1, 2), (1, 1), (1, 0), (0, 2), (0, 1), (0, 0)]
explanation = "Generate all coordinates in a grid in reverse order"
run_test(answer, range2d, explanation, *test)

test = [0, 0, 0, 3, 3, 3]
answer = [(0, 0, 0), (0, 0, 1), (0, 0, 2), (0, 1, 0), (0, 1, 1), (0, 1, 2), (0, 2, 0), (0, 2, 1), (0, 2, 2), (1, 0, 0), (1, 0, 1), (1, 0, 2), (1, 1, 0), (1, 1, 1), (1, 1, 2), (1, 2, 0), (1, 2, 1), (1, 2, 2), (2, 0, 0), (2, 0, 1), (2, 0, 2), (2, 1, 0), (2, 1, 1), (2, 1, 2), (2, 2, 0), (2, 2, 1), (2, 2, 2)]
explanation = "Generate all coordinates in a cube"
run_test(answer, range3d, explanation, *test)

test = (0, 0)
answer = (0, -1)
explanation = "Move upwards"
run_test(answer, move2d, explanation, *test, direction=UP)

test = (0, 0)
answer = (0, -2)
explanation = "Move two positions upwards"
run_test(answer, move2d, explanation, *test, direction=UP, n=2)

test = (0, 0)
answer = (2, 0)
explanation = "Move two positions right"
run_test(answer, move2d, explanation, *test, direction=RIGHT, n=2)

test = (0, 0)
answer = (0, 2)
explanation = "Move two positions down"
run_test(answer, move2d, explanation, *test, direction=DOWN, n=2)

test = (0, 0)
answer = (-2, 0)
explanation = "Move two positions left"
run_test(answer, move2d, explanation, *test, direction=LEFT, n=2)

test = 1
answer = [(0, -1), (1, 0), (0, 1), (-1, 0)]
explanation = "Get distance to all orthogonal neighbors"
run_test(answer, delta2d4, explanation, test)

test = 2
answer = [(0, -2), (2, 0), (0, 2), (-2, 0)]
explanation = "Get distance to all orthogonal neighbors two positions away"
run_test(answer, delta2d4, explanation, test)

test = 1
answer = [(0, -1), (1, -1), (1, 0), (1, 1), (0, 1), (-1, 1), (-1, 0), (-1, -1)]
explanation = "Get distance to all neighbors"
run_test(answer, delta2d8, explanation, test)

test = 2
answer = [(0, -2), (2, -2), (2, 0), (2, 2), (0, 2), (-2, 2), (-2, 0), (-2, -2)]
explanation = "Get distance to all neighbors two positions away"
run_test(answer, delta2d8, explanation, test)

test = (0, 0)
answer = [(0, -1), (1, 0), (0, 1), (-1, 0)]
explanation = "Get all orthogonal neighbors"
run_test(answer, neighbors2d4, explanation, *test)

test = (4, 6)
answer = [(4, 5), (5, 6), (4, 7), (3, 6)]
explanation = "Get all orthogonal neighbors somewhere"
run_test(answer, neighbors2d4, explanation, *test)

test = (0, 0)
answer = [(0, -2), (2, 0), (0, 2), (-2, 0)]
explanation = "Get all orthogonal neighbors two positions away"
run_test(answer, neighbors2d4, explanation, *test, n=2)

test = (0, 0)
answer = [(1, -1), (2, 0), (1, 1), (-1, 1), (-2, 0), (-1, -1)]
explanation = "Get all neighbors in a hexagon grid measured in half steps"

run_test(answer, neighbours2d6, explanation, *test)

test = (0, 0)
answer = [(0, -1), (1, -1), (1, 0), (1, 1), (0, 1), (-1, 1), (-1, 0), (-1, -1)]
explanation = "Get all neighbors"
run_test(answer, neighbors2d8, explanation, *test)
