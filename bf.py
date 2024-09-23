m = {
  '>': 1,
  '<': 2,
  '+': 3,
  '-': 4,
  '.': 5,
  ',': 6,
  '[': 7,
  ']': 8
}

def convert(prog):
  return [m[c] for c in prog]

def pp(prog):
  for i, n in enumerate(prog):
    print(f'prog[{i}] = {n};')

def bf(prog):
  pp(convert(prog))

prog = '++++++++[>++++++++<-]>.'
bf(prog)
