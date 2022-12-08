from sys import stdin

DATA = stdin.read().split('\n')


def read_disk_contents():
    structure = {}
    pointer = structure

    for line in DATA:
        args = line.split(' ')
        if args[0] == '$':
            if args[1] == 'cd':
                if args[2] == '/':
                    pointer = structure
                elif args[2] == '..':
                    pointer = pointer['..']
                else:
                    pointer = pointer[args[2]]
        elif args[0] == 'dir':
            pointer[args[1]] = {'..': pointer}
        else:
            pointer[args[1]] = int(args[0])

    return structure


def find_size(folder, cleanup=0):
    size = less_than_100000 = 0
    dir_options = []
    for name, contents in folder.items():
        if name == '..':
            continue
        if type(contents) == dict:
            sub_size, sub_less_than_100000, sub_options = find_size(contents, cleanup)
            less_than_100000 += sub_less_than_100000
            if sub_size <= 100000:
                less_than_100000 += sub_size
            if cleanup and sub_size >= cleanup:
                dir_options.append(sub_size)
            dir_options.extend(sub_options)
            size += sub_size
        else:
            size += contents
    return size, less_than_100000, dir_options


disk = read_disk_contents()

used_space, less_than_thousand_sum, _ = find_size(disk)
print(less_than_thousand_sum)

*_, options = find_size(disk, used_space - 40000000)
print(min(options))
