
enum Option {
  None;
  Some(Int);
}


enum List {
  Nil;
  Cons(Option, List);
}

fn id[X](x: X) -> X {
  x
};

let y = id(2);
let z = id(2.1);


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


fn main {
  print_int(t + w);
};
