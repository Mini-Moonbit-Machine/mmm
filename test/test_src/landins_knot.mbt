fn dummy(i : Int) -> Int {
  i
};

fn landins_knot(f : ((Int) -> Int) -> (Int) -> Int) -> (Int) -> Int {
  let arr = Array::make(1, dummy);
  fn aux(x) {
    arr[0](x)
  };
  let fixedpoint = f(aux);
  arr[0] = fixedpoint;
  fixedpoint
};

let factorial : (Int) -> Int = {
  fn g(f) {
    fn h(x) {
      if x == 0 {
        1
      } else {
        x * f(x - 1)
      }
    };

    h
  };

  landins_knot(g)
};

fn f1() -> Unit {
  print_int(factorial(5))
};

let with_effect : (Int) -> Int = {
  let a = factorial(5);
  fn g(x) {
    x + a
  };

  g
};

fn main {
  f1();
  print_int(with_effect(10));
  print_int(with_effect(31))
}
