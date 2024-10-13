import random
import os

len = 1000000
low = -2**31
high = 2**31-1
rands = [random.randint(low, high) for _ in range(len)]

# random test case generator for quicksort
with open('quicksort.in', 'w') as f:
  # generate numbers randomly
  f.write(str(len))
  f.write('\n')
  f.writelines('\n'.join([str(x) for x in rands]))

with open('quicksort.out', 'w') as f:
  rands.sort()
  f.write('\n'.join([str(x) for x in rands]))
  f.write('\n')

os.system('scp quicksort.out rvvm:~')
os.system('scp quicksort.in rvvm:~')