pub fn is_tail_rec(f : FuncDef) -> Bool {
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

fn subst_self_tail_rec_with_jump(fdef : FuncDef) -> FuncDef {
  let { name, ty, args, body } = fdef
  fn f(knf : Knf) {
    match knf {
      IfEq(x, y, e1, e2) => IfEq(x, y, f(e1), f(e2))
      IfLe(x, y, e1, e2) => IfLe(x, y, f(e1), f(e2))
      Let((x, t), e1, e2) => Let((x, t), e1, f(e2))
      LetTuple(xs, y, e) => LetTuple(xs, y, f(e))
      LetRec(fundef, e) => LetRec(fundef, f(e))
      LetJoin(fundef, e) => LetJoin(fundef, f(e))
      Apply(f, xs) => if f == name { Jump(f, xs) } else { Apply(f, xs) }
      Jump(_, _) => knf
      _ => knf
    }
  }

  { name, ty, args, body: f(body) }
}

fn recover_non_tail_apply(
  fdef : FuncDef,
  current_name : Name,
  orig_name : Name
) -> FuncDef {
  let { name, ty, args, body } = fdef
  fn f(knf : Knf) {
    match knf {
      IfEq(x, y, e1, e2) => IfEq(x, y, f(e1), f(e2))
      IfLe(x, y, e1, e2) => IfLe(x, y, f(e1), f(e2))
      Let((x, t), e1, e2) => Let((x, t), f(e1), f(e2))
      LetTuple(xs, y, e) => LetTuple(xs, y, f(e))
      LetRec(fundef, e) =>
        LetRec(recover_non_tail_apply(fundef, current_name, orig_name), f(e))
      LetJoin(fundef, e) =>
        LetJoin(recover_non_tail_apply(fundef, current_name, orig_name), f(e))
      Apply(f, xs) => if current_name == f { Apply(orig_name, xs) } else { knf }
      Jump(_, _) => knf
      _ => knf
    }
  }

  { name, ty, args, body: f(body) }
}

pub struct TailRecElim {
  tracer : @util.SubTracer
  knf : Knf
  knf_env : KnfEnv
}

pub fn TailRecElim::new(tracer : @util.SubTracer, knf : Knf) -> TailRecElim {
  let knf_env = KnfEnv::new(@typer.externals(), tracer)
  knf_env.init_counter_from_existing(knf)
  { tracer, knf, knf_env }
}

fn TailRecElim::func_tre(self : TailRecElim, fundef : FuncDef) -> FuncDef {
  self.tracer.println("func_tre: \{fundef.name} \{is_tail_rec(fundef)}")
  let { name, ty, args, body } = fundef
  let body = self.tre(body)
  let fundef = { ..fundef, body, }
  if is_tail_rec(fundef) {
    let jpdef = alpha_rename_def(self.knf_env, Env::new(), fundef)
    let jpdef = subst_self_tail_rec_with_jump(jpdef)
    let jpdef = recover_non_tail_apply(jpdef, jpdef.name, name)
    let body = Knf::LetJoin(
      jpdef,
      Knf::Jump(jpdef.name, args.map(fn(x) { x.0 })),
    )
    { name, ty, args, body }
  } else {
    fundef
  }
}

fn TailRecElim::tre(self : TailRecElim, knf : Knf) -> Knf {
  fn f(knf : Knf) {
    match knf {
      IfEq(x, y, e1, e2) => IfEq(x, y, f(e1), f(e2))
      IfLe(x, y, e1, e2) => IfLe(x, y, f(e1), f(e2))
      Let((x, t), e1, e2) => Let((x, t), f(e1), f(e2))
      LetTuple(xs, y, e) => LetTuple(xs, y, f(e))
      LetRec(fundef, e) => LetRec(self.func_tre(fundef), f(e))
      LetJoin(fundef, e) => LetJoin(self.func_tre(fundef), f(e))
      _ => knf
    }
  }

  f(knf)
}

pub fn TailRecElim::go(self : TailRecElim) -> Knf {
  let knf = self.tre(self.knf)
  self.tracer.record("knf_after_tre", knf.to_pretty_print())
  knf
}
