infix right 10 ** pow;
infix left 10 *** pow;

fn pow(x: Int, n: Int) -> Int {
  if n == 0 {
    1
  } else {
    x * pow(x, n - 1)
  }
};

fn main {
  print_int(2 ** 2 ** 3);
  print_char(10);
  print_int(2 *** 2 *** 3);
}
