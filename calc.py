from math import exp, log

def calc(scores, total=25, delta=1):
  ratio = exp(sum(log(score + delta) for score in scores) / len(scores)) - delta
  return ratio * total

def calc_old(scores, total=25):
  old_len = len(scores)
  scores = [score for score in scores if score > 0]
  new_len = len(scores)
  if new_len == 0:
    return 0
  return exp(sum(log(score) for score in scores) / new_len) * new_len / old_len * total

def sigmoid(x):
  return (0.8125 / (0.625 + exp(-x * 1.28093))) * 2 - 1

def calc_new(scores, total=25):
  return sum(sigmoid(score) for score in scores) / len(scores) * total

ds1 = [2.736, 2.628, 3.406, 0.216, 0.999, 1.539, 25.55]
ds2 = [2.736, 2.628, 3.406, 0.999, 1.539, 25.55]
ds3 = [25.55]

print("====================================")
print("使用旧算法计算 MMM 队分数")
print(calc_old(ds1))
print("使用旧算法计算 MMM 队故意 fail 掉低加速比 case 后的分数")
print(calc_old(ds2))
print("使用旧算法计算 MMM 队故意 fail 掉低加速比 case 后的分数 2")
print(calc_old(ds3))
print("使用新算法计算 MMM 队分数")
print(calc_new(ds1))