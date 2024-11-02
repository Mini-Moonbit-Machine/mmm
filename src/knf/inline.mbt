pub struct InlineEnv {
  knf_env : KnfEnv
  tracer : @util.SubTracer
}

pub fn InlineEnv::new(knf_env : KnfEnv, tracer : @util.SubTracer) -> InlineEnv {
  { knf_env, tracer }
}

typealias Env[T] = @immut/hashmap.T[Name, T]

pub enum CallFreq {
  Zero
  Once
  Many(Int)
  AsClosure
} derive(Show)

pub fn next(self : CallFreq) -> CallFreq {
  match self {
    Zero => Once
    Once => Many(2)
    Many(n) => Many(n + 1)
    AsClosure => AsClosure
  }
}

fn assoc(knf : Knf) -> Knf {
  match knf {
    IfEq(x, y, e1, e2) => IfEq(x, y, assoc(e1), assoc(e2))
    IfLe(x, y, e1, e2) => IfLe(x, y, assoc(e1), assoc(e2))
    Let(xt, e1, e2) => {
      fn insert(knf : Knf) -> Knf {
        match knf {
          Let(yt, e3, e4) => Let(yt, e3, insert(e4))
          LetRec(fundefs, e) => LetRec(fundefs, insert(e))
          LetTuple(yts, z, e) => LetTuple(yts, z, insert(e))
          e => Let(xt, e, assoc(e2))
        }
      }

      insert(assoc(e1))
    }
    LetRec(fundef, e) => LetRec(fundef, assoc(e))
    LetTuple(xts, y, e) => LetTuple(xts, y, assoc(e))
    _ => knf
  }
}

fn call_freq(knf : Knf, env : Env[Unit], freq : Map[Name, CallFreq]) -> Unit {
  fn deal_closure(xs : Array[Name]) -> Unit {
    for x in xs {
      match freq.get(x) {
        Some(_) => freq.set(x, AsClosure)
        None => ()
      }
    }
  }

  match knf {
    IfEq(_, _, e1, e2) => {
      call_freq(e1, env, freq)
      call_freq(e2, env, freq)
    }
    IfLe(_, _, e1, e2) => {
      call_freq(e1, env, freq)
      call_freq(e2, env, freq)
    }
    Let((_, _), e1, e2) => {
      call_freq(e1, env, freq)
      call_freq(e2, env, freq)
    }
    LetRec(fundef, e) => {
      let env = env.add(fundef.name, ())
      freq.set(fundef.name, Zero)
      call_freq(fundef.body, env, freq)
      call_freq(e, env, freq)
    }
    LetTuple(_, x, e) => {
      deal_closure([x])
      call_freq(e, env, freq)
    }
    Put(_, _, z) => deal_closure([z])
    Var(x) => deal_closure([x])
    Apply(f, xs) => {
      match freq.get(f) {
        Some(fq) => freq.set(f, fq.next())
        None => ()
      }
      deal_closure(xs)
    }
    Tuple(xs) => deal_closure(xs)
    ExternalFunctionApplication(_, xs) => deal_closure(xs)
    _ => ()
  }
}

fn size(knf : Knf) -> Int {
  match knf {
    IfEq(_, _, e1, e2) => 1 + size(e1) + size(e2)
    IfLe(_, _, e1, e2) => 1 + size(e1) + size(e2)
    Let(_, e1, e2) => 1 + size(e1) + size(e2)
    LetRec(fundef, e) => 1 + size(fundef.body) + size(e)
    LetTuple(_, _, e) => 1 + size(e)
    _ => 1
  }
}

fn alpha_rename(knf_env : KnfEnv, env : Env[Name], knf : Knf) -> Knf {
  fn f(x : Name) -> Name {
    env
    .find(x)
    .or(x)
    //.or_else(fn() { knf_env.tracer.abort("not found \{x} in \{env}") })
  }

  match knf {
    Unit => Unit
    Bool(b) => Bool(b)
    Int(i) => Int(i)
    Double(d) => Double(d)
    Neg(x) => Neg(f(x))
    Add(x, y) => Add(f(x), f(y))
    Sub(x, y) => Sub(f(x), f(y))
    Mul(x, y) => Mul(f(x), f(y))
    Div(x, y) => Div(f(x), f(y))
    FNeg(x) => FNeg(f(x))
    FAdd(x, y) => FAdd(f(x), f(y))
    FSub(x, y) => FSub(f(x), f(y))
    FMul(x, y) => FMul(f(x), f(y))
    FDiv(x, y) => FDiv(f(x), f(y))
    IfEq(x, y, e1, e2) =>
      IfEq(
        f(x),
        f(y),
        alpha_rename(knf_env, env, e1),
        alpha_rename(knf_env, env, e2),
      )
    IfLe(x, y, e1, e2) =>
      IfLe(
        f(x),
        f(y),
        alpha_rename(knf_env, env, e1),
        alpha_rename(knf_env, env, e2),
      )
    Let((xt, x), e1, e2) => {
      let xt2 = knf_env.new_named_temp(xt)
      let env2 = env.add(xt, xt2)
      Let(
        (xt2, x),
        alpha_rename(knf_env, env, e1),
        alpha_rename(knf_env, env2, e2),
      )
    }
    Var(x) => Var(f(x))
    LetRec(fundef, e) => {
      let new_fundef_name = knf_env.new_named_temp(fundef.name)
      let env = env.add(fundef.name, new_fundef_name)
      let args = []
      let mut env2 = env
      for arg in fundef.args {
        let na = (knf_env.new_named_temp(arg.0), arg.1)
        args.push(na)
        env2 = env2.add(arg.0, na.0)
      }
      LetRec(
        {
          name: new_fundef_name,
          ty: fundef.ty,
          args,
          body: alpha_rename(knf_env, env2, fundef.body),
        },
        alpha_rename(knf_env, env, e),
      )
    }
    Apply(func, xs) => Apply(f(func), xs.map(f))
    Tuple(xs) => Tuple(xs.map(f))
    LetTuple(xts, y, e) => {
      let new_xts = []
      let mut env = env
      for xt in xts {
        let (x, t) = xt
        let new_xt = (knf_env.new_named_temp(x), t)
        new_xts.push(new_xt)
        env = env.add(x, new_xt.0)
      }
      LetTuple(new_xts, f(y), alpha_rename(knf_env, env, e))
    }
    Get(x, y) => Get(f(x), f(y))
    Put(x, y, z) => Put(f(x), f(y), f(z))
    ExternalArray(x) => ExternalArray(f(x))
    ExternalFunctionApplication(name, xs) =>
      ExternalFunctionApplication(name, xs.map(f))
  }
}

pub fn InlineEnv::inline(
  self : InlineEnv,
  freq : Map[Name, CallFreq],
  env : Env[(Array[(Name, Type)], Knf)],
  knf : Knf
) -> Knf {
  match knf {
    IfEq(x, y, e1, e2) => {
      let e1 = self.inline(freq, env, e1)
      let e2 = self.inline(freq, env, e2)
      IfEq(x, y, e1, e2)
    }
    IfLe(x, y, e1, e2) => {
      let e1 = self.inline(freq, env, e1)
      let e2 = self.inline(freq, env, e2)
      IfLe(x, y, e1, e2)
    }
    Let(xt, e1, e2) => {
      let e1 = self.inline(freq, env, e1)
      let e2 = self.inline(freq, env, e2)
      Let(xt, e1, e2)
    }
    LetTuple(xts, y, e) => {
      let e = self.inline(freq, env, e)
      LetTuple(xts, y, e)
    }
    LetRec(fundef, e) => {
      let env = env.add(fundef.name, (fundef.args, fundef.body))
      let fundef = {
        name: fundef.name,
        ty: fundef.ty,
        args: fundef.args,
        body: self.inline(freq, env, fundef.body),
      }
      let e = self.inline(freq, env, e)
      LetRec(fundef, e)
    }
    Apply(f, xs) => {
      fn replace() {
        match env.find(f) {
          Some((params, body)) =>
            if f.name.to_string().contains("main") {
              knf
            } else {
              self.tracer.println("Knf inline \{f.name}")
              let mut env = Env::new()
              for i, param in params {
                env = env.add(param.0, xs[i])
              }
              alpha_rename(self.knf_env, env, body)
            }
          None => knf
        }
      }

      match freq.get(f) {
        Some(Once) => replace()
        Some(Many(n)) =>
          match env.find(f) {
            Some((_, body)) =>
              if n * size(body) < 20 {
                self.tracer.println("Inline threshold \{n} \{size(body)}")
                replace()
              } else {
                knf
              }
            None => knf
          }
        _ => knf
      }
    }
    _ => knf
  }
}

pub fn InlineEnv::dfe(
  self : InlineEnv,
  freq : Map[Name, CallFreq],
  env : Env[(Array[(Name, Type)], Knf)],
  knf : Knf
) -> Knf {
  match knf {
    IfEq(x, y, e1, e2) => {
      let e1 = self.dfe(freq, env, e1)
      let e2 = self.dfe(freq, env, e2)
      IfEq(x, y, e1, e2)
    }
    IfLe(x, y, e1, e2) => {
      let e1 = self.dfe(freq, env, e1)
      let e2 = self.dfe(freq, env, e2)
      IfLe(x, y, e1, e2)
    }
    Let(xt, e1, e2) => {
      let e1 = self.dfe(freq, env, e1)
      let e2 = self.dfe(freq, env, e2)
      Let(xt, e1, e2)
    }
    LetTuple(xts, y, e) => {
      let e = self.dfe(freq, env, e)
      LetTuple(xts, y, e)
    }
    LetRec(fundef, e) =>
      match freq.get(fundef.name) {
        Some(Zero) => self.dfe(freq, env, e)
        _ => {
          let env = env.add(fundef.name, (fundef.args, fundef.body))
          let fundef = {
            name: fundef.name,
            ty: fundef.ty,
            args: fundef.args,
            body: self.dfe(freq, env, fundef.body),
          }
          let e = self.dfe(freq, env, e)
          LetRec(fundef, e)
        }
      }
    _ => knf
  }
}

pub fn InlineEnv::go(self : InlineEnv, knf : Knf, n : Int) -> Knf {
  let freq = Map::new()
  let env = Env::new()
  call_freq(knf, env, freq)
  self.tracer.println("freq: \{freq}")
  let knf = self.inline(freq, Env::new(), knf)
  let freq = Map::new()
  call_freq(knf, env, freq)
  let knf = self.dfe(freq, Env::new(), knf)
  let knf = assoc(knf)
  self.tracer.record("knf_after_inline_\{n}", knf.to_pretty_print())
  knf
}
