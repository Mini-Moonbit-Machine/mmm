fn match_ref(x: Ref[Unit?]) -> Unit {
  match x.val {
    Some(r) => {
      x.val = Some(r)
    }
    None => ()
  }

}