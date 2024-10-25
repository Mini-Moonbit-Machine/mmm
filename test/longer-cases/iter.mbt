fn iter(arr: Array[Int], len: Int) -> ((Int) -> Bool) -> Bool {
  fn do(yield) -> Bool {
    fn next(i) -> Bool {
      if i <= len - 1 {
        if yield(arr[i]) {
          next(i + 1)
        } else {
          false
        }
      } else {
        false
      }
    };
    next(0)
  };
  do
};

fn each(iter: ((Int) -> Bool) -> Bool, f: (Int) -> Unit) -> Unit {
  fn yield(x) -> Bool {
    f(x);
    true
  };
  fn do() {
    if iter(yield) {
      do()
    } else {
      ()
    }
  };
  do()
};

fn main {
  let arr = Array::make(5, 1);
  arr[0] = 1;
  arr[1] = 2;
  arr[2] = 3;
  arr[3] = 4;
  arr[4] = 5;
  let f = iter(arr, 5);
  fn print(x) -> Unit {
    print_int(x)
  };
  each(f, print)
}