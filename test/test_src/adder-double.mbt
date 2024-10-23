fn make_adder(x: Double) -> (Int) -> Double {
  fn adder(y: Int) -> Double {
    x + float_of_int(y)
  };
  adder
};

fn main {
  let add = make_adder(2.14);
  print_int(int_of_float(add(1)))
};