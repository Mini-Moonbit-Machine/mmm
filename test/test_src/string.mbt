enum StrOpt {
  None;
  Some(String);
};


fn main {
  let r = Some("Moonbit");

  let t = match r {
    None => None;
    Some("OCaml") => Some("Hello 🐪");
    Some("Moonbit") => Some("Hello \u{6708}\u{5154}");
    Some(s) => None;
  };


  let w = match t {
    Some("Hello 月兔") => 1;
    None => 2;
    _ => 3;
  };
  print_int(w);
  

  let greet = " Hello 月兔 🐰 \x41 \u0030 \u{30} \u03B1 ";
  print_string(greet);
};
