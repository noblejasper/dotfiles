
import random

alnum = ['abcdefghijklmnopqrstuvwxz0123456789']
alnum_len = len(alnum)

all_num = 1000
char_num = 16
result_list = list()

def create_and_add_list(result_list):
    string = ''
    for char in range(char_num):
        index = random.choice(range(alnum_len))
        string += alnum[index]
    if string not in result_list:
        return [string]
    else:
        return create_and_add_list(result_list)

for i in range(all_num):
    result_list += create_and_add_list(result_list)

for string in result_list:
    print string[0:3] + "-" + string[3:7] + "-" + string[7:11] + "-" + string[11:15]
