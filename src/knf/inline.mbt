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
} derive(Show, Eq)

pub fn next(self : CallFreq) -> CallFreq {
  match self {
    Zero => Once
    Once => Many(2)
    Many(n) => Many(n + 1)
    AsClosure => AsClosure
  }
}

fn assoc_non_top(knf : Knf, top : Bool) -> Knf {
  match knf {
    IfEq(x, y, e1, e2) =>
      IfEq(x, y, assoc_non_top(e1, false), assoc_non_top(e2, false))
    IfLe(x, y, e1, e2) =>
      IfLe(x, y, assoc_non_top(e1, false), assoc_non_top(e2, false))
    Let(xt, e1, e2) =>
      if top {
        Let(xt, assoc_non_top(e1, false), assoc_non_top(e2, top))
      } else {
        fn insert(knf : Knf) -> Knf {
          match knf {
            Let(yt, e3, e4) => Let(yt, e3, insert(e4))
            LetRec(fundefs, e) => LetRec(fundefs, insert(e))
            LetTuple(yts, z, e) => LetTuple(yts, z, insert(e))
            e => Let(xt, e, assoc_non_top(e2, top))
          }
        }

        insert(assoc_non_top(e1, false))
      }
    LetRec(fundef, e) => {
      let { name, ty, args, body } = fundef
      let fundef = { name, ty, args, body: assoc_non_top(body, false) }
      LetRec(fundef, assoc_non_top(e, top))
    }
    LetTuple(xts, y, e) => LetTuple(xts, y, assoc_non_top(e, top))
    _ => knf
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
    LetRec(fundef, e) => {
      let { name, ty, args, body } = fundef
      let fundef = { name, ty, args, body: assoc(body) }
      LetRec(fundef, assoc(e))
    }
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

fn top_int_reduce(
  knf_env : KnfEnv,
  env : Env[(Type, Knf)],
  knf : Knf,
  top : Bool
) -> Knf {
  fn f(x : Name, cont : (Name) -> Knf) -> Knf {
    env
    .find(x)
    .map_or_else(
      fn() { cont(x) },
      fn(x) {
        let (ty, knf) = x
        let tmp = knf_env.new_temp()
        Let((tmp, ty), knf, cont(tmp))
      },
    )
  }

  fn fs(xs : Array[Name], cont : (Array[Name]) -> Knf) -> Knf {
    match xs {
      [] => cont([])
      [x, .. as rest] =>
        f(x, fn(x) { fs(rest.iter().to_array(), fn(xs) { cont([x, ..xs]) }) })
    }
  }

  match knf {
    Unit => Unit
    Bool(b) => Bool(b)
    Int(i) => Int(i)
    Double(d) => Double(d)
    Neg(x) => f(x, fn(x) { Neg(x) })
    Add(x, y) => f(x, fn(x) { f(y, fn(y) { Add(x, y) }) })
    Sub(x, y) => f(x, fn(x) { f(y, fn(y) { Sub(x, y) }) })
    Mul(x, y) => f(x, fn(x) { f(y, fn(y) { Mul(x, y) }) })
    Div(x, y) => f(x, fn(x) { f(y, fn(y) { Div(x, y) }) })
    FNeg(x) => f(x, fn(x) { FNeg(x) })
    FAdd(x, y) => f(x, fn(x) { f(y, fn(y) { FAdd(x, y) }) })
    FSub(x, y) => f(x, fn(x) { f(y, fn(y) { FSub(x, y) }) })
    FMul(x, y) => f(x, fn(x) { f(y, fn(y) { FMul(x, y) }) })
    FDiv(x, y) => f(x, fn(x) { f(y, fn(y) { FDiv(x, y) }) })
    IfEq(x, y, e1, e2) =>
      f(
        x,
        fn(x) {
          f(
            y,
            fn(y) {
              IfEq(
                x,
                y,
                top_int_reduce(knf_env, env, e1, false),
                top_int_reduce(knf_env, env, e2, false),
              )
            },
          )
        },
      )
    IfLe(x, y, e1, e2) =>
      f(
        x,
        fn(x) {
          f(
            y,
            fn(y) {
              IfLe(
                x,
                y,
                top_int_reduce(knf_env, env, e1, false),
                top_int_reduce(knf_env, env, e2, false),
              )
            },
          )
        },
      )
    Let((x, xt), e1, e2) =>
      if top {
        match e1 {
          Int(_) as y => {
            @util.t.println("top int reduce \{x} = \{y}")
            top_int_reduce(knf_env, env.add(x, (xt, y)), e2, top)
          }
          _ => {
            let e1 = top_int_reduce(knf_env, env, e1, false)
            Let((x, xt), e1, top_int_reduce(knf_env, env, e2, top))
          }
        }
      } else {
        let e1 = top_int_reduce(knf_env, env, e1, false)
        Let((x, xt), e1, top_int_reduce(knf_env, env, e2, top))
      }
    LetRec(fundef, e) => {
      let { name, ty, args, body } = fundef
      let fundef = {
        name,
        ty,
        args,
        body: top_int_reduce(knf_env, env, body, false),
      }
      LetRec(fundef, top_int_reduce(knf_env, env, e, top))
    }
    Var(x) => f(x, fn(x) { Var(x) })
    Tuple(xs) => fs(xs, fn(xs) { Tuple(xs) })
    LetTuple(xts, y, e) =>
      f(y, fn(y) { LetTuple(xts, y, top_int_reduce(knf_env, env, e, top)) })
    Get(x, y) => f(x, fn(x) { f(y, fn(y) { Get(x, y) }) })
    Put(x, y, z) => f(x, fn(x) { f(y, fn(y) { f(z, fn(z) { Put(x, y, z) }) }) })
    Apply(func, xs) => f(func, fn(func) { fs(xs, fn(xs) { Apply(func, xs) }) })
    ExternalArray(x) => f(x, fn(x) { ExternalArray(x) })
    ExternalFunctionApplication(name, xs) =>
      fs(xs, fn(xs) { ExternalFunctionApplication(name, xs) })
  }
}

fn beta_reduce(env : Env[Name], knf : Knf) -> Knf {
  fn f(x : Name) -> Name {
    env.find(x).or(x)
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
      IfEq(f(x), f(y), beta_reduce(env, e1), beta_reduce(env, e2))
    IfLe(x, y, e1, e2) =>
      IfLe(f(x), f(y), beta_reduce(env, e1), beta_reduce(env, e2))
    Let((x, xt), e1, e2) =>
      match beta_reduce(env, e1) {
        Var(y) => {
          @util.t.println("beta reduce \{x} = \{y}")
          beta_reduce(env.add(x, y), e2)
        }
        e1 => Let((x, xt), e1, beta_reduce(env, e2))
      }
    LetRec(fundef, e) => {
      let { name, ty, args, body } = fundef
      let fundef = { name, ty, args, body: beta_reduce(env, body) }
      let env = env.add(name, name)
      LetRec(fundef, beta_reduce(env, e))
    }
    Var(x) => Var(f(x))
    Tuple(xs) => Tuple(xs.map(f))
    LetTuple(xts, y, e) => LetTuple(xts, f(y), beta_reduce(env, e))
    Get(x, y) => Get(f(x), f(y))
    Put(x, y, z) => Put(f(x), f(y), f(z))
    Apply(func, xs) => Apply(f(func), xs.map(f))
    ExternalArray(x) => ExternalArray(f(x))
    ExternalFunctionApplication(name, xs) =>
      ExternalFunctionApplication(name, xs.map(f))
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
  env : Env[FuncDef],
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
      let env = env.add(fundef.name, fundef)
      let fundef = {
        name: fundef.name,
        ty: fundef.ty,
        args: fundef.args,
        body: self.inline(freq, env, fundef.body),
      }
      let env = env.add(fundef.name, fundef)
      let e = self.inline(freq, env, e)
      LetRec(fundef, e)
    }
    Apply(f, xs) => {
      fn replace() {
        match env.find(f) {
          Some({ args, body, .. }) => {
            self.tracer.println("Knf inline \{f.name}")
            let mut env = Env::new()
            for i, param in args {
              env = env.add(param.0, xs[i])
            }
            alpha_rename(self.knf_env, env, body)
          }
          None => {
            self.tracer.println("Knf inline not found \{f.name}")
            knf
          }
        }
      }

      if f.name.to_string().contains("main") {
        knf
      } else {
        match freq.get(f) {
          Some(Once) => replace()
          Some(Many(n)) =>
            match env.find(f) {
              Some({ args, body, ty, .. } as fundef) => {
                let fn_like_args = args
                  .filter(fn(x) { x.1.is_fn_like() })
                  .length() +
                  ty.is_fn_like().to_int()
                self.tracer.println("Fn like \{fn_like_args}")
                self.tracer.println("Inline threshold \{n} \{size(body)}")
                let small_size_and_freq = size(body) < 5 && n < 5
                let closure_with_non_rec = fn_like_args > 0 &&
                  not(is_loop(fundef)) &&
                  size(body) < 30
                if small_size_and_freq || closure_with_non_rec {
                  self.tracer.println("Inline decision accepted")
                  replace()
                } else {
                  self.tracer.println("Inline decision rejected")
                  knf
                }
              }
              None => knf
            }
          _ => knf
        }
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
  let knf = beta_reduce(Env::new(), knf)
  let knf = assoc(knf)
  let knf = self.inline(freq, Env::new(), knf)
  let freq = Map::new()
  call_freq(knf, env, freq)
  let knf = self.dfe(freq, Env::new(), knf)
  let knf = beta_reduce(Env::new(), knf)
  let knf = assoc(knf)
  let knf = beta_reduce(Env::new(), knf)
  let knf = assoc(knf)
  let knf = top_int_reduce(self.knf_env, Env::new(), knf, true)
  let knf = assoc_non_top(knf, true)
  self.tracer.record("knf_after_inline_\{n}", knf.to_pretty_print())
  knf
}
