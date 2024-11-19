//:extension

fn iter[T](arr: Array[T], len: Int) -> ((T) -> Bool) -> Bool {
  fn do(yield) {
    fn next(i) {
      if i <= len - 1 {
        if yield(arr[i]) { next(i + 1) } else { false }
      } else { false }
    };
    next(0)
  };
  do
};

fn each[T](iter: ((T) -> Bool) -> Bool, f: (T) -> Unit) -> Unit {
  fn yield(x) { f(x); true };
  fn do() {
    if iter(yield) { do() } else { () }
  };
  do()
};

fn main {
  let arr = Array::make(5, 1);
  arr[0] = 1; arr[1] = 2; arr[2] = 3;
  arr[3] = 4; arr[4] = 5;
  let int_arr_iter = iter(arr, 5);
  fn print_i(x) -> Unit {
    print_int(x)
  };
  each(int_arr_iter, print_i);
  let arr = Array::make(6, 0.0);
  arr[0] = 1.0; arr[1] = 2.0; arr[2] = 3.0;
  arr[3] = 4.0; arr[4] = 5.0; arr[5] = 6.0;
  let double_arr_iter = iter(arr, 6);
  fn print_d(x) -> Unit {
    print_int(int_of_float(x))
  };
  each(double_arr_iter, print_d)
}