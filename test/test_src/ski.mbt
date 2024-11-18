//:extension

fn s[X, Y, Z](x : (Z) -> (Y) -> X) -> ((Z) -> Y) -> (Z) -> X {
  fn f(y) {
    fn g(z) {
      x(z)(y(z))
    };

    g
  };

  f
};

fn k[X, Y](x : X) -> (Y) -> X {
  fn f(y){
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
