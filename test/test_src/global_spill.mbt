let x_1: Int = 1;
let x_2: Int = x_1 + 1;
let x_3: Int = x_2 + 1;
let x_4: Int = x_3 + 1;
let x_5: Int = x_4 + 1;
let x_6: Int = x_5 + 1;
let x_7: Int = x_6 + 1;
let x_8: Int = x_7 + 1;
let x_9: Int = x_8 + 1;
let x_10: Int = x_9 + 1;
let x_11: Int = x_10 + 1;
let x_12: Int = x_11 + 1;
let x_13: Int = x_12 + 1;
let x_14: Int = x_13 + 1;
let x_15: Int = x_14 + 1;

let dummy: Array[Int] = Array::make(1, 0);

fn do_nothing() -> () -> Unit {
  fn placeholder() -> Unit {
    dummy[0] = 0;
    ()
  };
  placeholder
};

fn print(n: Int) -> Unit {
  if n == 0 {
    ()
  } else {
    (do_nothing())();
    print_int(x_1 + x_2 + x_3 + x_4 + x_5 + x_6 + x_7 + x_8 + x_9 + x_10 + x_11 + x_12 + x_13 + x_14 + x_15);
    print(n - 1)
  }
};

fn main {
  print(10)
}