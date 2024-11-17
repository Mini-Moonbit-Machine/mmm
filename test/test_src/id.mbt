fn f[T](x: T) -> T {
  x
};

fn main {
  print_int(f(1000));
  print_int(int_of_float(f(1000.0)))
};