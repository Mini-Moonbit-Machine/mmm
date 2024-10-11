let len: Array[Int] = Array::make(1, 0);
fn init {
  len[0] = 1;
  ()
};
let len2: Array[Int] = Array::make(1, 0);
fn init {
  len2[0] = 2;
  ()
};

fn init {
  len[0] = 3;
  len2[0] = 4;
  ()
};

fn main {
  let _ = print_int(len[0]);
  let _ = print_endline();
  let _ = print_int(len2[0]);
  ()
}