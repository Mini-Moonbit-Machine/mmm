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

instance Print[Double] {
  print = { fn aux(x) { print_int(truncate(x)) }; aux };
};

class Num[T] {
  add : (T, T) -> T;
  mul : (T, T) -> T;
};

instance Num[Int] {
  add = { fn aux(x: Int, y) { x + y }; aux };
  mul = { fn aux(x: Int, y) { x * y }; aux };
};

instance Num[Double] {
  add = { fn aux(x: Double, y) { x + y }; aux };
  mul = { fn aux(x: Double, y) { x * y }; aux };
};

fn fused_mul_add[T: Num](x: T, y: T, z: T) -> T {
  add(mul(x, y), z)
};

fn println[P: Print](p : P) -> Unit {
  print(p);
  print_string("\n")
};

fn main {
  println(1);
  println("Hello Moonbit");
  println(1.0);
  println(fused_mul_add(1, 2, 3));
  println(fused_mul_add(1.0, 2.0, 3.0));
}
