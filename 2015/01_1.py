from functools import reduce
from operator import add

with open('01.txt') as f:
    data = f.read()
    floor = reduce(add, map(lambda x: 1 if x == '(' else -1, data))

print(floor)
