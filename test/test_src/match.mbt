
enum IntOpt {
  None;
  Some(Int);
};

enum IntOptList {
  Nil;
  Cons(IntOpt, IntOptList);
};

fn main {

  let p = Nil;
  let q = Cons(None, Nil);
  let r = Cons(Some(1), Nil);

  let _ = match 2 {
    1 => Nil;
    2 => q;
  };

  let t = match r {
    Nil => 0;
    Cons(None, Nil) => 1;
    Cons(Some(2), Nil) => 2;
    _ => 3;
  };


  let w = match r {
    Nil => 0;
    Cons(Some(v), _) => v;
    _ => 3;
  };
  print_int(t + w);
};
