//:has_read

fn swap(arr : Array[Int], i : Int, j : Int) -> Unit {
  let tmp = arr[i];
  arr[i] = arr[j];
  arr[j] = tmp;
  ()
};

fn output(arr: Array[Int], i: Int, len: Int) -> Unit {
  if i <= len - 1 {
    print_int(arr[i]);
    print_endline();
    output(arr, i + 1, len)
  } else {
    ()
  }
};

/// Used when the array is small enough (<=16) to avoid recursion overhead.
fn insertion_sort(arr : Array[Int], low : Int, high: Int) -> Unit {
  fn g(j : Int) -> Unit {
    if 1 + low <= j {
      if arr[j] <= arr[j - 1] - 1 {
        swap(arr, j, j - 1);
        g(j - 1)
      } else {
        ()
      }
    } else {
      ()
    }
  };

  fn f(i : Int) -> Unit {
    if i <= high - 1 {
      let j = i;
      g(j);
      f(i + 1)
    } else {
      ()
    }
  };

  f(low + 1)
};

fn partition(arr : Array[Int], pivot_index : Int, low: Int, high : Int) -> (Int, Bool) {
  swap(arr, pivot_index, high - 1);
  let pivot = arr[high - 1];
  let i = Array::make(1, low);
  let partitioned = Array::make(1, 1);
  fn aux(j : Int) {
    if j <= high - 2 {
      if arr[j] <= pivot - 1 {
        if i[0] == j {
          ()
        } else {
          swap(arr, i[0], j);
          partitioned[0] = 0
        };
        i[0] = i[0] + 1;
        ()
      } else {
        ()
      };
      aux(j + 1)
    } else {
      ()
    }
  };

  aux(low);
  swap(arr, i[0], high - 1);
  (i[0], partitioned[0] == 1)
};

/// Choose a pivot index for quick sort.
/// 
/// It avoids worst case performance by choosing a pivot that is likely to be close to the median.
/// 
/// Returns the pivot index and whether the array is likely sorted.
fn choose_pivot(arr : Array[Int], low: Int, high : Int) -> (Int, Bool) {
  let use_median_of_medians = 50;
  let swaps = Array::make(1, 0);
  let len = high - low;
  let b = low + len / 4 * 2;
  if 8 <= len {
    let a = low + len / 4 * 1;
    let c = low + len / 4 * 3;
    fn sort_2(a : Int, b : Int) {
      if arr[b] <= arr[a] - 1 {
        swap(arr, a, b);
        swaps[0] = swaps[0] + 1
      }
    };

    fn sort_3(a : Int, b : Int, c : Int) {
      sort_2(a, b);
      sort_2(b, c);
      sort_2(a, b)
    };

    if use_median_of_medians <= len - 1  {
      sort_3(a - 1, a, a + 1);
      sort_3(b - 1, b, b + 1);
      sort_3(c - 1, c, c + 1)
    };
    sort_3(a, b, c)
  };
  (b, swaps[0] == 0)
};


fn quick_sort(arr : Array[Int], low : Int, high : Int) -> Unit {
  let len = high - low;
  if len <= 16 {
    insertion_sort(arr, low, high)
  } else {
    let (pivot_index, likely_sorted) = choose_pivot(arr, low, high);
    let (pivot, partitioned) = partition(arr, pivot_index, low, high);
    quick_sort(arr, low, pivot);
    quick_sort(arr, pivot + 1, high)
  }
};

fn sort(x : Array[Int], len : Int) -> Unit {
  quick_sort(x, 0, len)
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
  sort(arr, len);
  output(arr, 0, len)
}