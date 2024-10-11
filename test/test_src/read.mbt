fn main {
  fn f() {
    let x = read_int();
    // check int min
    if x == -2147483648 {
      print_int(x)
    } else {
      print_int(x);
      f()
    }
  };
  f()
}