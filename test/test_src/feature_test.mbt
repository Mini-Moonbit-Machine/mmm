fn main {
  print_int(int_of_float(1.0 + 2.0));
  print_int(int_of_float(1.0 - 2.0));
  print_int(int_of_float(1.0 * 2.0));
  print_int(int_of_float(1.0 / 2.0));
  if 1.0 <= 2.0 {
    print_int(1)
  } else {
    print_int(0)
  };
  if 1.0 == 1.0 {
    print_int(1)
  } else {
    print_int(0)
  }
}