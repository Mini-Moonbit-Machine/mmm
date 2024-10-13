fn sum_prefix(arr: Array[Int], len: Int) -> Array[Int] {
  let res = Array::make(len, 0);
  fn sum_nested(acc: Int, x: Int) -> Unit {
    if x <= len - 1 {
      let s = acc + arr[x];
      res[x] = s;
      sum_nested(s, x + 1)
    } else {
      ()
    }
  };
  sum_nested(0, 0);
  res
};

fn main {
  let arr = Array::make(10, 4);
  print_int(sum_prefix(arr, 10)[0])
};
