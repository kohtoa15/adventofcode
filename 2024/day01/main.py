import sys
from collections import defaultdict

assert len(sys.argv) > 1
file = sys.argv[1]

a = []
b = []
right_index = defaultdict(int)
with open(file, "r") as f:
    for line in f.readlines():
        parts = line.split(" ")
        p1 = int(parts[0])
        p2 = int(parts[-1])
        a.append(p1)
        b.append(p2)
        right_index[p2] += 1

total_dist = sum(map(lambda a, b: abs(a - b), sorted(a), sorted(b)))
similarity = sum(map(lambda x: x * right_index[x], a))

print(f"Q1: {total_dist=}")
print(f"Q2: {similarity=}")
