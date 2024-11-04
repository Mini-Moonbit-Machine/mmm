typealias NameSet = @hashset.T[Name]

typealias ImmutNameSet = @immut/hashset.T[Name]

pub fn loop_inv_args(knf : Knf, name : Name, params : Array[Name]) -> NameSet {
  match knf {
    IfEq(_, _, e1, e2) | IfLe(_, _, e1, e2) =>
      loop_inv_args(e1, name, params).intersection(
        loop_inv_args(e2, name, params),
      )
    Let(_, _, e2) => loop_inv_args(e2, name, params)
    LetTuple(_, _, e2) => loop_inv_args(e2, name, params)
    LetRec(_, e2) => loop_inv_args(e2, name, params)
    LetJoin(_, e2) => loop_inv_args(e2, name, params)
    Apply(f, args) =>
      if f == name {
        let set = NameSet::new()
        for i, arg in args {
          if arg == params[i] {
            set.insert(arg)
          }
        }
        set
      } else {
        NameSet::from_iter(params.iter())
      }
    Jump(_, _) => NameSet::new()
    _ => NameSet::from_iter(params.iter())
  }
}

pub fn loop_inv(knf : Knf, env : ImmutNameSet) -> Bool {
  match knf {
    Var(x) => env.contains(x)
    Get(x, y) => env.contains(x) && env.contains(y)
    _ => false
  }
}

fn bind_all_with_map(
  k : Knf,
  knf_env : KnfEnv,
  let_list : Array[Binding],
  map : Env[Name]
) -> Knf {
  let mut ret = k
  let rlist = let_list.rev()
  for item in rlist {
    ret = match item {
      Binding::Let(x, ty, e) => {
        let y = knf_env.new_named_temp(x)
        Let(
          (y, ty),
          alpha_rename(knf_env, map, e),
          alpha_rename(knf_env, map.add(x, y), ret),
        )
      }
      _ => @util.t.abort("unexpected binding")
    }
  }
  ret
}

pub fn hoist_inv_from_def(
  f : FuncDef,
  tracer : @util.SubTracer
) -> (FuncDef, Array[Binding]) {
  let { name, ty, args, body } = f
  let inv_args = loop_inv_args(body, name, args.map(fn(x) { x.0 }))
  let inv = ImmutNameSet::from_iter(inv_args.iter())
  let bindings = []
  fn aux(knf : Knf, inv) -> Knf {
    match knf {
      IfEq(x, y, e1, e2) => IfEq(x, y, aux(e1, inv), aux(e2, inv))
      IfLe(x, y, e1, e2) => IfLe(x, y, aux(e1, inv), aux(e2, inv))
      Let((x, t), e1, e2) => {
        let e1 = aux(e1, inv)
        if loop_inv(e1, inv) && t.is_ptr_like() {
          bindings.push(Binding::Let(x, t, e1))
          tracer.println("hoist: \{x}")
          aux(e2, inv)
        } else {
          Let((x, t), e1, aux(e2, inv))
        }
      }
      LetTuple(xs, y, e) => LetTuple(xs, y, aux(e, inv))
      LetRec(fundef, e) => LetRec(fundef, aux(e, inv))
      _ => knf
    }
  }

  let body = aux(body, inv)
  let new_args = []
  for binding in bindings {
    guard let Binding::Let(x, ty, _) = binding else {
      _ => @util.t.abort("unexpected binding")
    }
    new_args.push((x, ty))
  }
  fn aux2(knf : Knf) -> Knf {
    match knf {
      IfEq(x, y, e1, e2) => IfEq(x, y, aux2(e1), aux2(e2))
      IfLe(x, y, e1, e2) => IfLe(x, y, aux2(e1), aux2(e2))
      Let((x, t), e1, e2) => Let((x, t), aux2(e1), aux2(e2))
      LetTuple(xs, y, e) => LetTuple(xs, y, aux2(e))
      LetRec(fundef, e) => LetRec(fundef, aux2(e))
      Apply(f, xs) =>
        if f == name {
          Apply(f, xs + new_args.map(fn(x) { x.0 }))
        } else {
          Apply(f, xs)
        }
      _ => knf
    }
  }

  let body = aux2(body)
  let args = args + new_args
  (FuncDef::{ name, ty, args, body }, bindings)
}

pub struct LICM {
  tracer : @util.SubTracer
  knf : Knf
}

pub fn LICM::new(tracer : @util.SubTracer, knf : Knf) -> LICM {
  { tracer, knf }
}

pub fn LICM::go(self : LICM, id : Int) -> Knf {
  let knf_env = KnfEnv::new(@typer.externals(), self.tracer)
  knf_env.init_counter_from_existing(self.knf)
  fn hoist(knf : Knf, env : Env[(FuncDef, Array[Binding])]) -> Knf {
    match knf {
      IfEq(x, y, e1, e2) => IfEq(x, y, hoist(e1, env), hoist(e2, env))
      IfLe(x, y, e1, e2) => IfLe(x, y, hoist(e1, env), hoist(e2, env))
      Let((x, t), e1, e2) => Let((x, t), hoist(e1, env), hoist(e2, env))
      LetTuple(xs, y, e) => LetTuple(xs, y, hoist(e, env))
      LetRec(fundef, e) => {
        let (fundef, bindings) = hoist_inv_from_def(fundef, self.tracer)
        let { name, ty, args, body } = fundef
        let fundef = { name, ty, args, body: hoist(body, env) }
        LetRec(fundef, hoist(e, env.add(fundef.name, (fundef, bindings))))
      }
      LetJoin(fundef, e) => {
        let (fundef, bindings) = hoist_inv_from_def(fundef, self.tracer)
        let { name, ty, args, body } = fundef
        let fundef = { name, ty, args, body: hoist(body, env) }
        LetJoin(fundef, hoist(e, env.add(fundef.name, (fundef, bindings))))
      }
      Apply(f, xs) =>
        match env[f] {
          Some((fdef, bindings)) => {
            let mut map = Env::new()
            for i, arg in xs {
              map = map.add(fdef.args[i].0, arg)
            }
            let names = bindings.map(
              fn(b) {
                match b {
                  Binding::Let(x, _, _) => x
                  _ => @util.t.abort("unexpected binding")
                }
              },
            )
            bind_all_with_map(Apply(f, xs + names), knf_env, bindings, map)
          }
          None => knf
        }
      _ => knf
    }
  }

  let knf = hoist(self.knf, Env::new())
  let knf = assoc(knf)
  self.tracer.record("knf_after_licm_\{id}", knf.to_pretty_print())
  knf
}
