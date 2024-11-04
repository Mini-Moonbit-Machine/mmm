pub struct InlineEnv {
  knf : Knf
  knf_env : KnfEnv
  tracer : @util.SubTracer
}

pub fn InlineEnv::new(tracer : @util.SubTracer, knf : Knf) -> InlineEnv {
  let knf_env = KnfEnv::new(@typer.externals(), tracer)
  knf_env.init_counter_from_existing(knf)
  { knf, knf_env, tracer }
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
    LetRec(fundef, e) | LetJoin(fundef, e) => {
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
    Apply(f, xs) | Jump(f, xs) => {
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
    LetJoin(fundef, e) => 1 + size(fundef.body) + size(e)
    LetTuple(_, _, e) => 1 + size(e)
    _ => 1
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
    LetJoin(fundef, e) => {
      let { name, ty, args, body } = fundef
      let fundef = { name, ty, args, body: beta_reduce(env, body) }
      LetJoin(fundef, beta_reduce(env, e))
    }
    Var(x) => Var(f(x))
    Tuple(xs) => Tuple(xs.map(f))
    LetTuple(xts, y, e) => LetTuple(xts, f(y), beta_reduce(env, e))
    Get(x, y) => Get(f(x), f(y))
    Put(x, y, z) => Put(f(x), f(y), f(z))
    Apply(func, xs) => Apply(f(func), xs.map(f))
    Jump(func, xs) => Jump(f(func), xs.map(f))
    ExternalArray(x) => ExternalArray(f(x))
    ExternalFunctionApplication(name, xs) =>
      ExternalFunctionApplication(name, xs.map(f))
  }
}

fn alpha_rename_def(
  knf_env : KnfEnv,
  env : Env[Name],
  funcdef : FuncDef
) -> FuncDef {
  let { name, ty, args, body } = funcdef
  let new_fundef_name = knf_env.new_named_temp(name)
  let env = env.add(name, new_fundef_name)
  let new_args = []
  let mut env2 = env
  for arg in args {
    let na = (knf_env.new_named_temp(arg.0), arg.1)
    new_args.push(na)
    env2 = env2.add(arg.0, na.0)
  }
  let args = new_args
  let name = new_fundef_name
  let body = alpha_rename(knf_env, env2, body)
  { name, ty, args, body }
}

fn alpha_rename_def_k(
  knf_env : KnfEnv,
  env : Env[Name],
  funcdef : FuncDef,
  k : (Knf) -> Knf
) -> FuncDef {
  let { name, ty, args, body } = funcdef
  let new_fundef_name = knf_env.new_named_temp(name)
  let env = env.add(name, new_fundef_name)
  let new_args = []
  let mut env2 = env
  for arg in args {
    let na = (knf_env.new_named_temp(arg.0), arg.1)
    new_args.push(na)
    env2 = env2.add(arg.0, na.0)
  }
  let args = new_args
  let name = new_fundef_name
  let body = alpha_rename_k(knf_env, env2, body, k)
  { name, ty, args, body }
}

fn alpha_rename(knf_env : KnfEnv, env : Env[Name], knf : Knf) -> Knf {
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
      let new_fundef = alpha_rename_def(knf_env, env, fundef)
      let env = env.add(fundef.name, new_fundef.name)
      LetRec(new_fundef, alpha_rename(knf_env, env, e))
    }
    LetJoin(fundef, e) => {
      let new_fundef = alpha_rename_def(knf_env, env, fundef)
      let env = env.add(fundef.name, new_fundef.name)
      LetJoin(new_fundef, alpha_rename(knf_env, env, e))
    }
    Apply(func, xs) => Apply(f(func), xs.map(f))
    Jump(func, xs) => Jump(f(func), xs.map(f))
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

fn alpha_rename_k(
  knf_env : KnfEnv,
  env : Env[Name],
  knf : Knf,
  k : (Knf) -> Knf
) -> Knf {
  fn f(x : Name) -> Name {
    env.find(x).or(x)
  }

  match knf {
    Unit => Unit |> k
    Bool(b) => Bool(b) |> k
    Int(i) => Int(i) |> k
    Double(d) => Double(d) |> k
    Neg(x) => Neg(f(x)) |> k
    Add(x, y) => Add(f(x), f(y)) |> k
    Sub(x, y) => Sub(f(x), f(y)) |> k
    Mul(x, y) => Mul(f(x), f(y)) |> k
    Div(x, y) => Div(f(x), f(y)) |> k
    FNeg(x) => FNeg(f(x)) |> k
    FAdd(x, y) => FAdd(f(x), f(y)) |> k
    FSub(x, y) => FSub(f(x), f(y)) |> k
    FMul(x, y) => FMul(f(x), f(y)) |> k
    FDiv(x, y) => FDiv(f(x), f(y)) |> k
    IfEq(x, y, e1, e2) =>
      IfEq(
        f(x),
        f(y),
        alpha_rename_k(knf_env, env, e1, k),
        alpha_rename_k(knf_env, env, e2, k),
      )
    IfLe(x, y, e1, e2) =>
      IfLe(
        f(x),
        f(y),
        alpha_rename_k(knf_env, env, e1, k),
        alpha_rename_k(knf_env, env, e2, k),
      )
    Let((xt, x), e1, e2) => {
      let xt2 = knf_env.new_named_temp(xt)
      let env2 = env.add(xt, xt2)
      alpha_rename_k(
        knf_env,
        env,
        e1,
        fn(e1) { Let((xt2, x), e1, alpha_rename_k(knf_env, env2, e2, k)) },
      )
    }
    Var(x) => Var(f(x)) |> k
    LetRec(fundef, e) => {
      let new_fundef = alpha_rename_def_k(knf_env, env, fundef, fn(k) { k })
      let env = env.add(fundef.name, new_fundef.name)
      LetRec(new_fundef, alpha_rename_k(knf_env, env, e, k))
    }
    LetJoin(fundef, e) => {
      let new_fundef = alpha_rename_def_k(knf_env, env, fundef, k)
      let env = env.add(fundef.name, new_fundef.name)
      LetJoin(new_fundef, alpha_rename_k(knf_env, env, e, k))
    }
    Apply(func, xs) => Apply(f(func), xs.map(f)) |> k
    Jump(func, xs) => Jump(f(func), xs.map(f)) // discard
    Tuple(xs) => Tuple(xs.map(f)) |> k
    LetTuple(xts, y, e) => {
      let new_xts = []
      let mut env = env
      for xt in xts {
        let (x, t) = xt
        let new_xt = (knf_env.new_named_temp(x), t)
        new_xts.push(new_xt)
        env = env.add(x, new_xt.0)
      }
      LetTuple(new_xts, f(y), alpha_rename_k(knf_env, env, e, k))
    }
    Get(x, y) => Get(f(x), f(y)) |> k
    Put(x, y, z) => Put(f(x), f(y), f(z)) |> k
    ExternalArray(x) => ExternalArray(f(x)) |> k
    ExternalFunctionApplication(name, xs) =>
      ExternalFunctionApplication(name, xs.map(f)) |> k
  }
}

fn jp_count(knf : Knf) -> Int {
  match knf {
    IfEq(_, _, e1, e2) => jp_count(e1) + jp_count(e2)
    IfLe(_, _, e1, e2) => jp_count(e1) + jp_count(e2)
    Let(_, e1, e2) => jp_count(e1) + jp_count(e2)
    LetJoin(fundef, e) => 1 + jp_count(e) + jp_count(fundef.body)
    LetRec(fundef, e) => jp_count(e) + jp_count(fundef.body)
    LetTuple(_, _, e) => jp_count(e)
    _ => 0
  }
}

pub fn InlineEnv::inline(
  self : InlineEnv,
  freq : Map[Name, CallFreq],
  env : Env[FuncDef],
  fun : Name?,
  knf : Knf,
  cont : (Knf) -> Knf
) -> Knf {
  match knf {
    IfEq(x, y, e1, e2) => {
      let e1 = self.inline(freq, env, fun, e1, cont)
      let e2 = self.inline(freq, env, fun, e2, cont)
      IfEq(x, y, e1, e2)
    }
    IfLe(x, y, e1, e2) => {
      let e1 = self.inline(freq, env, fun, e1, cont)
      let e2 = self.inline(freq, env, fun, e2, cont)
      IfLe(x, y, e1, e2)
    }
    Let(xt, e1, e2) =>
      self.inline(
        freq,
        env,
        fun,
        e1,
        fn(e1) { Let(xt, e1, self.inline(freq, env, fun, e2, cont)) },
      )
    LetTuple(xts, y, e) => {
      let e = self.inline(freq, env, fun, e, cont)
      LetTuple(xts, y, e)
    }
    LetRec({ name, ty, args, body } as fundef, e) => {
      let env = env.add(name, fundef)
      let fun = Some(name)
      LetRec(
        { name, ty, args, body: self.inline(freq, env, fun, body, fn(x) { x }) },
        self.inline(freq, env, fun, e, cont),
      )
    }
    LetJoin({ name, ty, args, body }, e) =>
      LetJoin(
        { name, ty, args, body: self.inline(freq, env, fun, body, cont) },
        self.inline(freq, env, fun, e, cont),
      )
    Apply(f, xs) => {
      if f.to_string().contains("main") {
        return cont(knf)
      }
      fn replace(fundef : FuncDef) {
        let k = cont(Unit)
        let k = k.to_pretty_print()
        self.tracer.println("Knf inline \{f.name} \{Show::to_string(k)}")
        let mut env = Env::new()
        for i, param in fundef.args {
          env = env.add(param.0, xs[i])
        }
        alpha_rename_k(self.knf_env, env, fundef.body, cont)
      }

      fn inline(
        args : Array[(@types.Name, @types.Type)],
        body,
        ty : @types.Type,
        freq : CallFreq,
        fundef : FuncDef
      ) {
        self.tracer.println("Inline threshold \{size(body)}")
        let fn_like_args = args.filter(fn(x) { x.1.is_fn_like() }).length() +
          ty.is_fn_like().to_int()
        self.tracer.println("Fn like \{fn_like_args}")
        let jp_count = jp_count(body)
        self.tracer.println("Join point count \{jp_count}")
        let n = match freq {
          Once => 1
          Many(n) => n
          Zero => 0
          AsClosure => 100000
        }
        if size(body) < 5 && n < 5 {
          self.tracer.println("Inline decision \{f} accepted")
          replace(fundef)
        } else {
          self.tracer.println("Inline decision \{f} rejected")
          cont(knf)
        }
      }

      match env.find(f) {
        Some({ body, args, ty, .. } as fundef) =>
          match freq.get(f) {
            Some(Once as f) => inline(args, body, ty, f, fundef)
            Some(Many(_) as f) => inline(args, body, ty, f, fundef)
            _ => cont(knf)
          }
        None => cont(knf)
      }
    }
    Jump(_, _) => knf // discard
    _ => cont(knf)
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
    LetJoin(fundef, e) =>
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
          LetJoin(fundef, e)
        }
      }
    _ => knf
  }
}

pub fn InlineEnv::go(self : InlineEnv, n : Int) -> Knf {
  let freq = Map::new()
  let knf = beta_reduce(Env::new(), self.knf)
  let knf = assoc(knf)
  call_freq(knf, Env::new(), freq)
  self.tracer.println("freq: \{freq}")
  let knf = self.inline(freq, Env::new(), None, knf, fn(k) { k })
  let freq = Map::new()
  call_freq(knf, Env::new(), freq)
  let knf = self.dfe(freq, Env::new(), knf)
  self.tracer.record("knf_after_inline_\{n}", knf.to_pretty_print())
  let knf = beta_reduce(Env::new(), knf)
  let knf = assoc(knf)
  let knf = beta_reduce(Env::new(), knf)
  let knf = assoc(knf)
  let knf = beta_reduce(Env::new(), knf)
  let knf = assoc(knf)
  self.tracer.record("knf_after_beta_reduction", knf.to_pretty_print())
  knf
}
