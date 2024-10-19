fn swap(arr: Array[Int], i: Int, j: Int) -> Unit {
  let tmp = arr[i];
  arr[i] = arr[j];
  arr[j] = tmp;
};

fn partition(arr: Array[Int], low: Int, high: Int) -> Int {
  let pivot = arr[high];
  let i = Array::make(1, low - 1);
  fn l(j) {
    if j <= high - 1 {
      if arr[j] <= pivot {
        i[0] = i[0] + 1;
        swap(arr, i[0], j)
      };
      l(j + 1)
    } else {
      ()
    }
  };
  l(low);
  i[0] = i[0] + 1;
  swap(arr, i[0], high);
  i[0]
};
fn quicksort(arr: Array[Int], low: Int, high: Int) -> Unit {
  if low <= high - 1 {
    let pi = partition(arr, low, high);
    quicksort(arr, low, pi - 1);
    quicksort(arr, pi + 1, high)
  } else {
    ()
  }
};

fn main {
  let len = read_int();
  let arr = Array::make(len, 0);
  fn input(i: Int) {
    if i <= len - 1 {
      arr[i] = read_int();
      input(i + 1)
    } else {
      ()
    }
  };
  input(0);
  quicksort(arr, 0, len - 1);
  fn output(i: Int) {
    if i <= len - 1 {
      print_int(arr[i]);
      print_endline();
      output(i + 1)
    } else {
      ()
    }
  };
  output(0)
}

