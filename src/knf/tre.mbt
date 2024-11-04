pub fn is_loop(f : FuncDef) -> Bool {
  tail_rec_or_trivial(f.body, f.name) && has_self_rec(f.body, f.name)
}

pub fn tail_rec_or_trivial(knf : Knf, name : Name) -> Bool {
  match knf {
    IfEq(_, _, e1, e2) | IfLe(_, _, e1, e2) =>
      tail_rec_or_trivial(e1, name) && tail_rec_or_trivial(e2, name)
    Let(_, _, e2) => tail_rec_or_trivial(e2, name)
    LetTuple(_, _, e2) => tail_rec_or_trivial(e2, name)
    LetRec(_, e2) => tail_rec_or_trivial(e2, name)
    LetJoin(_, e2) => tail_rec_or_trivial(e2, name)
    Apply(f, _) => f == name
    Jump(_, _) => false
    _ => true
  }
}

pub fn has_self_rec(knf : Knf, name : Name) -> Bool {
  match knf {
    IfEq(_, _, e1, e2) | IfLe(_, _, e1, e2) =>
      has_self_rec(e1, name) || has_self_rec(e2, name)
    Let(_, _, e2) => has_self_rec(e2, name)
    LetTuple(_, _, e2) => has_self_rec(e2, name)
    LetRec(_, e2) => has_self_rec(e2, name)
    LetJoin(_, e2) => has_self_rec(e2, name)
    Apply(f, _) => f == name
    Jump(_, _) => false
    _ => false
  }
}

fn _func_tre(fdef : FuncDef) -> FuncDef? {
  if is_loop(fdef) {
    fn aux(knf : Knf) {
      match knf {
        IfEq(x, y, e1, e2) => IfEq(x, y, aux(e1), aux(e2))
        IfLe(x, y, e1, e2) => IfLe(x, y, aux(e1), aux(e2))
        Let((x, t), e1, e2) => Let((x, t), aux(e1), aux(e2))
        LetTuple(xs, y, e) => LetTuple(xs, y, aux(e))
        LetRec(fundef, e) => LetRec(fundef, aux(e))
        LetJoin(fundef, e) => LetJoin(fundef, aux(e))
        _ => knf
      }
    }

    let { name, ty, args, body } = fdef
    let body = aux(body)
    Some({ name, ty, args, body })
  } else {
    None
  }
}
