//:extension

class Print[T] {
  print : T -> Unit;
};


instance Print[Int] {
  print = { fn aux(x) { print_int(x) }; aux };
};

instance Print[String] {
  print = { fn aux(x) { print_string(x) }; aux };
}