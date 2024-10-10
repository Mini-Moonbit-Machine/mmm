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
  let arr = Array::make(6, 0);
  arr[0] = 10;
  arr[1] = 7;
  arr[2] = 8;
  arr[3] = 9;
  arr[4] = 1;
  arr[5] = 5;
  quicksort(arr, 0, 5);
  print_int(arr[0]);
  print_endline();
  print_int(arr[1]);
  print_endline();
  print_int(arr[2]);
  print_endline();
  print_int(arr[3]);
  print_endline();
  print_int(arr[4]);
  print_endline();
  print_int(arr[5])
}