import sys
sys.setrecursionlimit(100000)

def search(floor):
    if data.pop(0) == '(':
        floor += 1
    else:
        floor -= 1

    return 1 if floor == -1 else 1 + search(floor)

with open('01.txt') as f:
    data = list(f.read())

search(0)
