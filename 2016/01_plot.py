import sys

from matplotlib import pyplot as plt

input = sys.stdin.readlines()

def parse(s):
    _, x, y = s.strip().replace('(','').replace(')','').split()
    return (int(x),int(y))

data = [parse(line) for line in input[1:]]

x, y = zip(*data)

plt.plot(*zip(*data))
plt.show()
