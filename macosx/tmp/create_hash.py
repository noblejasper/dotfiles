import random

base = 'abcdefghijklmnopqrstuvwxyz01234567890'
sample_list = []
limit_num = 5000
str_num   = 15

def _create_hash():
    h = ''
    for i in range(str_num):
        h += random.choice(base)
    return h

for i in range(limit_num):
    while 1:
        h = _create_hash()
        if h in sample_list:
            continue
        sample_list += [h]
        print h
        break
