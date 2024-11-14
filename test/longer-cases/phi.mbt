fn rec(x: Int, y: Int) -> Unit {
  if x == y {
    rec(y, x + 1)
  } else {
    ()
  }
};

fn main {
    rec(0, 0);
};