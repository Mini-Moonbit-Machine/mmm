import json

keys = [
  "use_xthead",
  "knf_inline",
  "knf_licm",
  "knf_lift",
  "knf_unbox",
  "core_inline",
  "core_licm",
  "core_lvn",
  "core_2_fo",
  "core_2_jtbl",
  "core_2_lvn",
]

with open('mmm.json') as f:
  conf = json.load(f)
  conf_str = "conf_"
  for key in keys:
    if key in conf:
      x = conf[key]
      if x == True:
        x = "t"
      elif x == False:
        x = "f"
      conf_str += f"{x}"
  print(conf_str)