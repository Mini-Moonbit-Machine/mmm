//:extension

enum IntOpt {
  None;
  Some(Int);
};

class Print[T] {
  print: T -> Unit;
};

instance Print[IntOpt] {
  print = { 
    fn aux(x) {
      match x {
        None => print_string("None");
        Some(i) => {
          print_string("Some(");
          print_int(i);
          print_string(")");
        };
      };
    };
    aux
  };
};

fn println[X: Print](x: X) -> Unit {
  print(x);
  print_string("\n");
};


fn main {
  let xs = [None, Some(1), Some(2)];
  println(xs[0]);
  println(xs[1]);
  println(xs[2]);
};