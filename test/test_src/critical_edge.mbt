fn f() -> Int {
  if 6 <= 0 {
    1
  } else {
    if 6 <= 10 {
      2
    } else {
      3
    }
  }
};

fn main {
  print_int(f())
};