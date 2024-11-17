fn s[X, Y, Z](x : (Z) -> (Y) -> X) -> ((Z) -> Y) -> (Z) -> X {
  fn f(y : (Z) -> Y) {
    fn g(z : Z) {
      x(z)(y(z))
    };

    g
  };

  f
};

fn k[X, Y](x : X) -> (Y) -> X {
  fn f(y : Y) -> X {
    x
  };

  f
};

fn i[X](x : X) -> X {
  x
};

fn main {
  let rev_apply = s(k(s(i)))(k);
  fn printi(x : Int) {
    print_int(x)
  };
  rev_apply(2)(printi)
};
