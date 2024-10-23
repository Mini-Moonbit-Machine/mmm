fn return_i_d() -> (Int, Double) {
  (1, 2.0)
};

fn return_i_d_i() -> (Int, Double, Int) {
  (1, 2.0, 3)
};

fn return_d_i_i() -> (Double, Int, Int) {
  (2.0, 3, 4)
};

fn return_i_i_i() -> (Int, Int, Int) {
  (1, 2, 3)
};

fn main {
  let (i, d) = return_i_d();
  print_int(i);
  print_int(int_of_float(d));
  let (i, d, i2) = return_i_d_i();
  print_int(i);
  print_int(int_of_float(d));
  print_int(i2);
  let (d, i, i2) = return_d_i_i();
  print_int(int_of_float(d));
  print_int(i);
  print_int(i2);
  let (i, i2, i3) = return_i_i_i();
  print_int(i);
  print_int(i2);
  print_int(i3);
  ()
}

