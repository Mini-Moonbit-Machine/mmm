//:extension

class Print[T] {
  print : T -> Unit;
};

instance Print[Int] {
  print = { fn aux(x) { print_int(x) }; aux };
};

instance Print[String] {
  print = { fn aux(x) { print_string(x) }; aux };
};

fn print_generic[P: Print, X](p : P, _dummy: X) -> Unit {
  print(p)
};

fn main {
  print_generic(1, 0.1);
  print_generic("\n", ());
  print_generic("Hello Moonbit", 2);
}

