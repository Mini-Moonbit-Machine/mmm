fn useless(x: Int) -> Int {
  let y = x;
  10-1
};
fn dummy(x: Int) -> Int {
  x
};

fn f(x: Int) -> Int {
  fn g(y: Int) -> Int {
    x
  };
  if true {
    f(1)
  } else {
    g(2)
  } 
};

fn main {
  ()
}
