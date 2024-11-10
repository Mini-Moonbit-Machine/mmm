fn small() -> Int {
  5
};

fn rec(n : Int) -> Unit {
  if n == 42 {
    let _ = small();
    ()
  } else {
    rec(n+1)
  }
};

fn dummy() -> Unit {
  let _ = small();
  let _ = small();
  let _ = small();
  let _ = small();
  let _ = small();
  ()
};

fn main {
  dummy();
  rec(0)
};