enum TupleFreq {
  Destructed
  MayEscape
} derive(Eq, Show)

fn tuple_freq(knf : Knf, env : Env[Unit], freq : Map[Name, TupleFreq]) -> Unit {
  fn deal_fisrt_class_usage(xs : Array[Name]) -> Unit {
    for x in xs {
      freq.set(x, MayEscape)
    }
  }

  fn deal_destructed(xs : Array[Name]) -> Unit {
    for x in xs {
      match freq.get(x) {
        Some(MayEscape) => ()
        _ => freq.set(x, Destructed)
      }
    }
  }

  match knf {
    IfEq(_, _, e1, e2) => {
      tuple_freq(e1, env, freq)
      tuple_freq(e2, env, freq)
    }
    IfLe(_, _, e1, e2) => {
      tuple_freq(e1, env, freq)
      tuple_freq(e2, env, freq)
    }
    Let((_, Type::Tuple(_)), e1, e2) => {
      tuple_freq(e1, env, freq)
      tuple_freq(e2, env, freq)
    }
    Let((_, _), e1, e2) => {
      tuple_freq(e1, env, freq)
      tuple_freq(e2, env, freq)
    }
    LetRec(fundef, e) => {
      tuple_freq(fundef.body, env, freq)
      tuple_freq(e, env, freq)
    }
    LetTuple(_, x, e) => {
      deal_destructed([x])
      tuple_freq(e, env, freq)
    }
    Put(_, _, z) => deal_fisrt_class_usage([z])
    Var(x) => deal_fisrt_class_usage([x])
    Apply(_, xs) => deal_fisrt_class_usage(xs)
    Tuple(xs) => deal_fisrt_class_usage(xs)
    ExternalFunctionApplication(_, xs) => deal_fisrt_class_usage(xs)
    _ => ()
  }
}

fn is_tail_tuple(knf : Knf, n : Int) -> Bool {
  match knf {
    IfEq(_, _, e1, e2) => is_tail_tuple(e1, n) && is_tail_tuple(e2, n)
    IfLe(_, _, e1, e2) => is_tail_tuple(e1, n) && is_tail_tuple(e2, n)
    Let(_, _, e2) => is_tail_tuple(e2, n)
    LetRec(_, e) => is_tail_tuple(e, n)
    LetTuple(_, _, e) => is_tail_tuple(e, n)
    Tuple(xs) => xs.length() < n
    _ => false
  }
}

pub struct Unbox {
  tracer : @util.SubTracer
  knf : Knf
}

pub fn Unbox::new(tracer : @util.SubTracer, knf : Knf) -> Unbox {
  { tracer, knf }
}

pub fn go(self : Unbox) -> Knf {
  let knf = self.knf
  let freq = Map::new()
  tuple_freq(knf, Env::new(), freq)
  self.tracer.println("Tuple freq: \{freq}")
  fn aux(knf : Knf, env : Env[Unit], unbox_tail) -> Knf {
    match knf {
      IfEq(x, y, e1, e2) =>
        IfEq(x, y, aux(e1, env, unbox_tail), aux(e2, env, unbox_tail))
      IfLe(x, y, e1, e2) =>
        IfLe(x, y, aux(e1, env, unbox_tail), aux(e2, env, unbox_tail))
      Let((x, t), e1, e2) =>
        if freq.get(x) == Some(Destructed) && is_tail_tuple(e1, 4) {
          Let((x, t), aux(e1, env, true), aux(e2, env.add(x, ()), unbox_tail))
        } else {
          Let((x, t), aux(e1, env, unbox_tail), aux(e2, env, unbox_tail))
        }
      LetRec({ body, .. } as fundef, e) => {
        let body = aux(body, env, false)
        LetRec({ ..fundef, body, }, aux(e, env, unbox_tail))
      }
      LetTuple(xts, y, e) =>
        if freq.get(y) == Some(Destructed) &&
          env.find(y) == Some(()) &&
          xts.length() < 4 {
          LetUnboxedTuple(xts, y, aux(e, env, unbox_tail))
        } else {
          LetTuple(xts, y, aux(e, env, unbox_tail))
        }
      Tuple(xs) => if unbox_tail { UnboxedTuple(xs) } else { Tuple(xs) }
      _ => knf
    }
  }

  let knf = aux(knf, Env::new(), false)
  self.tracer.record("knf_after_unbox", knf.to_pretty_print())
  knf
}
