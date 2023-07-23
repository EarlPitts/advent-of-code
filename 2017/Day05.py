import sys
from enum import Enum

Dir = Enum('Dir', ['LEFT', 'RIGHT'])
Sign = Enum('Sign', ['POSITIVE', 'NEGATIVE'])

def mk_ins(l):
    return (l,0)

def get_val(z):
    l,i = z
    return l[i]

def move_left(z,n):
    l,i = z
    return (l,i-n)

def move_right(z,n):
    l,i = z
    return (l,i+n)

def modify(z,v):
    l,i = z
    l[i] = v

def move(dir, n, z):
    if dir == 'LEFT':
        return move_left(z,n)
    elif dir == 'RIGHT':
        return move_right(z,n)

def sign(n):
    if n >= 0:
        return 'POSITIVE'
    else:
        return 'NEGATIVE'

def step(z):
    val = get_val(z)
    dir = 'LEFT' if sign(val) == 'NEGATIVE' else 'RIGHT'
    incr = -1 if val >= 3 else 1
    modify(z, (val + incr))
    return move(dir, abs(val), z)

def solve(z, n=0):
    try:
        while True:
            z = step(z)
            n = n + 1
    except:
        print(n)

def main():
    data = [int(line) for line in sys.stdin]
    z = mk_ins(data)
    solve(z)

main()
