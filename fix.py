import re

new_functions = set()
regexp = re.compile("defun ca-([\w-]+)")

for line in open("conf.org"):
    m = regexp.search(line)
    if m:
        new_functions.add(m.group(1))


def find_in_set(func_set, st):
    for func in func_set:
        if func in st:
            return func


result = []
for line in open('conf.org'):
    func = find_in_set(new_functions, line)
    if func and ('ca' not in line):
        line = line.replace(func, 'ca-' + func)
    result.append(line)

print ''.join(result)
