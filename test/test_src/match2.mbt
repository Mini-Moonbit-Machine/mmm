enum X {
  A(X, X);
  B(Int, Int);
  C;
};

fn main {
  let i = match (true, false) {
    (true, _) => 1; // here
    (_, true) => 2;
    _ => 3;
  };
  let j = match (i <= 1, i <= 2, i <= 3) {
    (true, _, _) => 1; // here
    (_, true, _) => 2;
    (_, _, true) => 3;
    _ => 4;
  };
  let k = match (j, j + 1, j + 2, j + 3) {
    (1, 2, 3, 5) => 1;
    (1, 2, 3, 6) => 2;
    (1, 2, 3, 7) => 3;
    _ => 4; // here
  };
  let m = match A(B(1, 2), C) {
    A(B(a, b), B(c, d)) => 1;
    A(C, B(a, b)) => 2;
    A(B(1, 3), C) => 3;
    A(B(1, 2), C) => 4; // here
    B(_, _) => 5;
    C => 6;
  };
  print_int(i);
  print_int(j);
  print_int(k);
  print_int(m);
};
