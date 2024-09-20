fn f(l: Int, m: Int, n: Int) -> Unit {
  fn cap1() {
    fn cap2() {
      fn cap3() {
        l + m + n
      };
      let _ = cap3();
      l + m
    };
    let _ = cap2();
    l
  }
};

fn main {
  ()
}
