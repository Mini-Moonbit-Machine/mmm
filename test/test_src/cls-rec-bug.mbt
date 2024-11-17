fn f(x: Int) -> Int {
  if 0 <= x {
    let _ = f(-1);
    1
  } else {
    -1
  }
};

fn main {
  print_int(f(10));
  print_int(f(-10));
  ()
};
