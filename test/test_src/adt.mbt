//:extension

enum IntList {
  Nil;
  Cons(Int, IntList);
};

fn int_array_to_list(arr: Array[Int], len: Int) -> IntList {
  fn aux(i: Int) -> IntList {
    if i == len {
      Nil
    } else {
      Cons(arr[i], aux(i + 1))
    }
  };
  aux(0)
};

fn print_int_list(lst: IntList) -> Unit {
  match lst {
    Nil => ();
    Cons(h, t) => {
      print_int(h);
      print_int_list(t)
    };
  }
};

fn main {
  let arr = Array::make(5, 1);
  arr[0] = 1;
  arr[1] = 2;
  arr[2] = 3;
  arr[3] = 4;
  arr[4] = 5;
  let lst = int_array_to_list(arr, 5);
  print_int_list(lst)
};