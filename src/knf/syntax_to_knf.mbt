pub typealias LocalEnv = @immut/hashmap.T[String, Type]

pub typealias NameEnv = @immut/hashmap.T[String, Name]

struct KnfEnv {
  mut counter : Int
  externals : LocalEnv
}

pub fn KnfEnv::new(externals : @immut/hashmap.T[String, Type]) -> KnfEnv {
  { counter: 1, externals }
}

pub fn KnfEnv::init_counter_from_existing(
  self : KnfEnv,
  existing : Knf
) -> Unit {
  self.counter = knf_max_counter(existing) + 1
}

fn KnfEnv::new_temp(self : KnfEnv) -> Name {
  let temp = Name::slot_only(self.counter)
  self.counter += 1
  temp
}

fn KnfEnv::new_named_temp(self : KnfEnv, name : Name) -> Name {
  let counter = self.counter
  self.counter += 1
  { ..name, slot: counter }
}

pub fn KnfEnv::to_knf(self : KnfEnv, s : @types.Syntax) -> Knf!Failure {
  let const_1 = try {
    Name::from_string!("<const_1>")
  } catch {
    _ => fail!("invalid constant name")
  }
  let let_list = [Binding::Let(const_1, @types.Type::Int, Int(1))]
  let name_env = NameEnv::new().add("<const_1>", const_1)
  let local_env = @typer.externals().add("<const_1>", @types.Type::Int)
  let r = self.build_knf!(s, let_list, name_env, local_env)
  bind_all(r)
}

enum Binding {
  Let(Name, Type, Knf)
  LetRec(FuncDef)
  LetTuple(Array[(Name, Type)], Name)
}

typealias LetList = Array[Binding]

typealias KnfResult = (Knf, Type, LetList)

fn bind_all(kr : KnfResult) -> Knf {
  let (k, _, let_list) = kr
  let mut ret = k
  let rlist = let_list.rev()
  for item in rlist {
    ret = match item {
      Binding::Let(x, ty, e) => Let((x, ty), e, ret)
      Binding::LetRec(f) => LetRec(f, ret)
      Binding::LetTuple(xs, y) => LetTuple(xs, y, ret)
    }
  }
  ret
}

fn materialize_knf(self : KnfEnv, knf : KnfResult, let_list : LetList) -> Name {
  match knf {
    (Knf::Var(x), _, _) => x
    (k, ty, _) => {
      let temp = self.new_temp()
      let_list.push(Binding::Let(temp, ty, k))
      temp
    }
  }
}

fn make_name(self : KnfEnv, name : String) -> Name!Failure {
  try {
    if name == "_" {
      self.new_temp()
    } else {
      Name::from_string!(name)
    }
  } catch {
    _ => fail!("invalid variable name: \{name}")
  }
}

fn build_knf(
  self : KnfEnv,
  node : @types.Syntax,
  let_list : LetList,
  env : NameEnv,
  ty_env : LocalEnv
) -> KnfResult!Failure {
  match node {
    Unit => (Unit, Unit, let_list)
    Int(i) => (Int(i), Int, let_list)
    Bool(b) => (Int(if b { 1 } else { 0 }), Int, let_list)
    Double(d) => (Double(d), Double, let_list)
    Var(x) =>
      match (env.find(x), ty_env.find(x)) {
        (Some(name), Some(ty)) => (Var(name), ty, let_list)
        _ => fail!("unbound variable: \{x}")
      }
    Tuple(xs) => {
      let ys = []
      let tys = []
      for x in xs {
        let r = self.build_knf!(x, let_list, env, ty_env)
        let tmp = self.materialize_knf(r, let_list)
        ys.push(tmp)
        tys.push(r.1)
      }
      (Tuple(ys), Tuple(tys), let_list)
    }
    Not(xs) => {
      let r = self.build_knf!(xs, let_list, env, ty_env)
      let tmp = self.materialize_knf(r, let_list)
      (IfEq(tmp, env.find("<const_1>").unwrap(), Int(0), Int(1)), Int, let_list)
    }
    Array(len, elem) => {
      let r1 = self.build_knf!(len, let_list, env, ty_env)
      let len = self.materialize_knf(r1, let_list)
      let r2 = self.build_knf!(elem, let_list, env, ty_env)
      let elem = self.materialize_knf(r2, let_list)
      let call = match r2.1.repr() {
        Int => ExternalFunctionApplication("minimbt_create_array", [len, elem])
        Double =>
          ExternalFunctionApplication("minimbt_create_float_array", [len, elem])
        t =>
          if t.is_float_like() {
            ExternalFunctionApplication(
              "minimbt_create_float_array",
              [len, elem],
            )
          } else if t.is_ptr_like() {
            ExternalFunctionApplication("minimbt_create_ptr_array", [len, elem])
          } else {
            fail!("invalid array element type: \{r2.1}")
          }
      }
      (call, Array(r2.1), let_list)
    }
    Neg(xs, kind=Some(@types.Kind::Int)) => {
      let r = self.build_knf!(xs, let_list, env, ty_env)
      let tmp = self.materialize_knf(r, let_list)
      (Neg(tmp), Int, let_list)
    }
    Neg(xs, kind=Some(@types.Kind::Double)) => {
      let r = self.build_knf!(xs, let_list, env, ty_env)
      let tmp = self.materialize_knf(r, let_list)
      (FNeg(tmp), Double, let_list)
    }
    Neg(_, ~kind) => fail!("invalid kind: \{kind}")
    App(Var(f) as fv, xs) =>
      if self.externals.find(f).is_empty() == false {
        let args = []
        for x in xs {
          let r = self.build_knf!(x, let_list, env, ty_env)
          let tmp = self.materialize_knf(r, let_list)
          args.push(tmp)
        }
        (
          ExternalFunctionApplication("minimbt_" + f, args),
          self.externals.find(f).unwrap(),
          let_list,
        )
      } else {
        let r = self.build_knf!(fv, let_list, env, ty_env)
        let fv = self.materialize_knf(r, let_list)
        let args = []
        for x in xs {
          let r = self.build_knf!(x, let_list, env, ty_env)
          let tmp = self.materialize_knf(r, let_list)
          args.push(tmp)
        }
        (
          Apply(fv, args),
          match r.1.repr() {
            @types.Type::Fun(_, t) => t
            _ => fail!("not a function: \{r.0} : \{r.1}")
          },
          let_list,
        )
      }
    App(fv, xs) => {
      let r = self.build_knf!(fv, let_list, env, ty_env)
      let fv = self.materialize_knf(r, let_list)
      let args = []
      for x in xs {
        let r = self.build_knf!(x, let_list, env, ty_env)
        let tmp = self.materialize_knf(r, let_list)
        args.push(tmp)
      }
      (
        Apply(fv, args),
        match r.1.repr() {
          @types.Type::Fun(_, t) => t
          _ => fail!("not a function: \{r.0} : \{r.1}")
        },
        let_list,
      )
    }
    Get(x, y) => {
      let r1 = self.build_knf!(x, let_list, env, ty_env)
      let x = self.materialize_knf(r1, let_list)
      let r2 = self.build_knf!(y, let_list, env, ty_env)
      let y = self.materialize_knf(r2, let_list)
      (
        Get(x, y),
        match r1.1.repr() {
          @types.Type::Array(t) => t
          _ => fail!("not a array: \{r1.0} : \{r1.1}")
        },
        let_list,
      )
    }
    // It's possible to omit the 3 cases, but it's better to fuse them here
    If(Eq(o1, o2), y, z) => {
      let r1 = self.build_knf!(o1, let_list, env, ty_env)
      let o1 = self.materialize_knf(r1, let_list)
      let r2 = self.build_knf!(o2, let_list, env, ty_env)
      let o2 = self.materialize_knf(r2, let_list)
      let ll1 = []
      let r3 = self.build_knf!(y, ll1, env, ty_env)
      let ll2 = []
      let r4 = self.build_knf!(z, ll2, env, ty_env)
      (IfEq(o1, o2, bind_all(r3), bind_all(r4)), r3.1, let_list)
    }
    If(LE(o1, o2), y, z) => {
      let r1 = self.build_knf!(o1, let_list, env, ty_env)
      let o1 = self.materialize_knf(r1, let_list)
      let r2 = self.build_knf!(o2, let_list, env, ty_env)
      let o2 = self.materialize_knf(r2, let_list)
      let ll1 = []
      let r3 = self.build_knf!(y, ll1, env, ty_env)
      let ll2 = []
      let r4 = self.build_knf!(z, ll2, env, ty_env)
      (IfLe(o1, o2, bind_all(r3), bind_all(r4)), r3.1, let_list)
    }
    If(Not(x), y, z) => self.build_knf!(If(x, z, y), let_list, env, ty_env)
    If(x, y, z) => {
      let r = self.build_knf!(x, let_list, env, ty_env)
      let x = self.materialize_knf(r, let_list)
      let ll1 = []
      let r1 = self.build_knf!(y, ll1, env, ty_env)
      let ll2 = []
      let r2 = self.build_knf!(z, ll2, env, ty_env)
      (
        IfEq(x, env.find("<const_1>").unwrap(), bind_all(r1), bind_all(r2)),
        r1.1,
        let_list,
      )
    }
    Prim(o1, o2, op, kind=Some(@types.Kind::Int)) => {
      let r1 = self.build_knf!(o1, let_list, env, ty_env)
      let x = self.materialize_knf(r1, let_list)
      let r2 = self.build_knf!(o2, let_list, env, ty_env)
      let y = self.materialize_knf(r2, let_list)
      (
        match op {
          Add => Add(x, y)
          Sub => Sub(x, y)
          Mul => Mul(x, y)
          Div => Div(x, y)
        },
        Int,
        let_list,
      )
    }
    Prim(o1, o2, op, kind=Some(@types.Kind::Double)) => {
      let r1 = self.build_knf!(o1, let_list, env, ty_env)
      let x = self.materialize_knf(r1, let_list)
      let r2 = self.build_knf!(o2, let_list, env, ty_env)
      let y = self.materialize_knf(r2, let_list)
      (
        match op {
          Add => FAdd(x, y)
          Sub => FSub(x, y)
          Mul => FMul(x, y)
          Div => FDiv(x, y)
        },
        Double,
        let_list,
      )
    }
    Prim(_, _, _, ~kind) => fail!("invalid kind: \{kind}")
    Eq(o1, o2) => {
      let r1 = self.build_knf!(o1, let_list, env, ty_env)
      let x = self.materialize_knf(r1, let_list)
      let r2 = self.build_knf!(o2, let_list, env, ty_env)
      let y = self.materialize_knf(r2, let_list)
      (IfEq(x, y, Int(1), Int(0)), Int, let_list)
    }
    LE(o1, o2) => {
      let r1 = self.build_knf!(o1, let_list, env, ty_env)
      let x = self.materialize_knf(r1, let_list)
      let r2 = self.build_knf!(o2, let_list, env, ty_env)
      let y = self.materialize_knf(r2, let_list)
      (IfLe(x, y, Int(1), Int(0)), Int, let_list)
    }
    Let((x, ty), e1, e2) => {
      let r1 = self.build_knf!(e1, let_list, env, ty_env)
      let e1 = self.materialize_knf(r1, let_list)
      let env = env.add(x, e1)
      let ty_env = ty_env.add(x, ty)
      let r2 = self.build_knf!(e2, let_list, env, ty_env)
      (r2.0, r2.1, let_list)
    }
    LetRec({ name, args, body }, e) => {
      let ty = name.1
      let orig_name = name.0
      let name = self.make_name!(orig_name)
      let mut env = env
      let mut ty_env = ty_env
      env = env.add(orig_name, name)
      ty_env = ty_env.add(
        orig_name,
        @types.Type::Fun(args.map(fn(x) { x.1 }), ty),
      )
      let mut env2 = env
      let mut ty_env2 = ty_env
      let new_binders = []
      let inner_let_list = []
      for item in args {
        let (x, ty) = item
        let x_name = self.make_name!(x)
        let tmp = self.new_named_temp(x_name)
        new_binders.push((tmp, ty))
        env2 = env2.add(x, tmp)
        ty_env2 = ty_env2.add(x, ty)
      }
      let r = self.build_knf!(body, inner_let_list, env2, ty_env2)
      let new_def = FuncDef::{ name, ty, args: new_binders, body: bind_all(r) }
      let_list.push(Binding::LetRec(new_def))
      let r = self.build_knf!(e, let_list, env, ty_env)
      (r.0, r.1, let_list)
    }
    LetTuple(xs, y, e) => {
      let r1 = self.build_knf!(y, let_list, env, ty_env)
      let y = self.materialize_knf(r1, let_list)
      let mut env2 = env
      let mut ty_env2 = ty_env
      let new_binders = []
      for item in xs {
        let (x, ty) = item
        let x_name = self.make_name!(x)
        let tmp = self.new_named_temp(x_name)
        new_binders.push((tmp, ty))
        env2 = env2.add(x, tmp)
        ty_env2 = ty_env2.add(x, ty)
      }
      let_list.push(Binding::LetTuple(new_binders, y))
      let r2 = self.build_knf!(e, let_list, env2, ty_env2)
      (r2.0, r2.1, let_list)
    }
    Put(dst, idx, src) => {
      let r1 = self.build_knf!(dst, let_list, env, ty_env)
      let dst = self.materialize_knf(r1, let_list)
      let r2 = self.build_knf!(idx, let_list, env, ty_env)
      let idx = self.materialize_knf(r2, let_list)
      let r3 = self.build_knf!(src, let_list, env, ty_env)
      let src = self.materialize_knf(r3, let_list)
      (Put(dst, idx, src), Unit, let_list)
    }
  }
}
