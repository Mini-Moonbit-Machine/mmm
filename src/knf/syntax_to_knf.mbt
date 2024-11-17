pub typealias LocalEnv = @immut/hashmap.T[String, Type]

pub typealias NameEnv = @immut/hashmap.T[String, Name]

pub typealias BindEnv = @immut/hashmap.T[Occurrence, (Name, Type)]

struct KnfEnv {
  mut counter : Int
  mut named_tys : LocalEnv
  externals : LocalEnv
  tracer : @util.SubTracer
}

pub fn KnfEnv::new(
  externals : @immut/hashmap.T[String, Type],
  tracer : @util.SubTracer
) -> KnfEnv {
  { counter: 1, named_tys: LocalEnv::new(), externals, tracer }
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

pub fn KnfEnv::to_knf(
  self : KnfEnv,
  named_tys : LocalEnv,
  s : @types.Syntax
) -> Knf!Failure {
  let let_list = []
  let name_env = NameEnv::new()
  // externals are handled by @typer.externals()
  let local_env = LocalEnv::new()
  self.named_tys = named_tys
  let r = self.build_knf!(s, let_list, name_env, local_env)
  let knf = bind_all(r)
  self.tracer.record("knf", knf.to_pretty_print())
  knf
}

enum Binding {
  Let(Name, Type, Knf)
  LetRec(FuncDef)
  LetTuple(Array[(Name, Type)], Name)
  LetUnboxedTuple(Array[(Name, Type)], Name)
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
      Binding::LetUnboxedTuple(xs, y) => LetUnboxedTuple(xs, y, ret)
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

fn materialize_knf_with_name(
  self : KnfEnv,
  knf : KnfResult,
  let_list : LetList,
  name : String
) -> Name!Failure {
  match knf {
    (Knf::Var(x), _, _) => x
    (k, ty, _) => {
      let temp = self.new_named_temp(
        try {
          if name == "_" {
            self.new_temp()
          } else {
            Name::from_string!(name)
          }
        } catch {
          _ => fail!("invalid variable name: \{name}")
        },
      )
      let_list.push(Binding::Let(temp, ty, k))
      temp
    }
  }
}

fn make_name(self : KnfEnv, name : String) -> Name {
  if name == "_" {
    self.new_temp()
  } else {
    let new_id = self.counter
    self.counter += 1
    Name::name_and_slot(name, new_id)
  }
}

fn decision_tree_to_knf(
  self : KnfEnv,
  tree : DecisionTree,
  let_list : LetList,
  bind_env : BindEnv,
  env : NameEnv,
  ty_env : LocalEnv
) -> KnfResult!Failure {
  self.tracer.println("decision_tree_to_knf: \{tree}")
  match tree {
    Succeed(~binds, ~action) => {
      let mut env2 = env
      let mut ty_env2 = ty_env
      let new_binders = []
      for occnm in binds {
        let (occ, name) = occnm
        let (knf_name, ty) = bind_env
          .find(occ)
          .or_else(self.tracer.abort("unbound occurrence: \{occ}"))
        env2 = env2.add(name, knf_name)
        ty_env2 = ty_env2.add(name, ty)
      }
      let r = self.build_knf!(action, let_list, env, ty_env)
      r
    }
    Fail =>
      // fallthrough
      (Unit, Unit, let_list)
    Switch(~occur, ~cases, ~default) => {
      let defaultk = self.decision_tree_to_knf!(
        default, let_list, bind_env, env, ty_env,
      )
      let mut switchks = defaultk
      let mut bind_env2 = bind_env
      // fold from inner match cases
      for case in cases.rev() {
        let (subpat, subtree) = case
        let checkk = match subpat {
          Unit => {
            let x = match bind_env2.find(occur) {
              Some(x) => x
              None => fail!("unbound occurrence: \{occur}")
            }
            let r = self.build_knf!(Unit, [], env, ty_env)
            let o = self.materialize_knf(r, [])
            let subtreek = self.decision_tree_to_knf!(
              subtree,
              [],
              bind_env,
              env,
              ty_env,
            )
            (
              IfEq(x.0, o, bind_all(subtreek), bind_all(switchks)),
              subtreek.1,
              let_list,
            )
          }
          BoolLit(~bval) => {
            let x = match bind_env2.find(occur) {
              Some(x) => x
              None => fail!("unbound occurrence: \{occur}")
            }
            let r = self.build_knf!(Bool(bval), [], env, ty_env)
            let o = self.materialize_knf(r, [])
            let subtreek = self.decision_tree_to_knf!(
              subtree,
              [],
              bind_env,
              env,
              ty_env,
            )
            (
              IfEq(x.0, o, bind_all(subtreek), bind_all(switchks)),
              subtreek.1,
              let_list,
            )
          }
          IntLit(~ival) => {
            let x = match bind_env2.find(occur) {
              Some(x) => x
              None => fail!("unbound occurrence: \{occur}")
            }
            let r = self.build_knf!(Int(ival), [], env, ty_env)
            let o = self.materialize_knf(r, [])
            let subtreek = self.decision_tree_to_knf!(
              subtree,
              [],
              bind_env,
              env,
              ty_env,
            )
            (
              IfEq(x.0, o, bind_all(subtreek), bind_all(switchks)),
              subtreek.1,
              let_list,
            )
          }
          DoubleLit(~dval) => {
            let x = match bind_env2.find(occur) {
              Some(x) => x
              None => fail!("unbound occurrence: \{occur}")
            }
            let r = self.build_knf!(Double(dval), [], env, ty_env)
            let o = self.materialize_knf(r, [])
            let subtreek = self.decision_tree_to_knf!(
              subtree,
              [],
              bind_env,
              env,
              ty_env,
            )
            (
              IfEq(x.0, o, bind_all(subtreek), bind_all(switchks)),
              subtreek.1,
              let_list,
            )
          }
          Ctor(~name, ~csize) => {
            // unpack occurrences and bind name
            let ctor_ty = ty_env.find(name).unwrap()
            match ctor_ty {
              @types.Type::Enum(_, variants) => {
                let variant_id = variants
                  .search_by(fn(ctor) { name == ctor.0 })
                  .unwrap()
                let (_, args_ty) = variants[variant_id]
                let x = match bind_env.find(occur) {
                  Some(x) => x
                  None => fail!("unbound occurrence: \{occur}")
                }
                // ctor id: variant index
                let r = self.build_knf!(
                  Int(variant_id.to_int64()),
                  [],
                  env,
                  ty_env,
                )
                let o = self.materialize_knf(r, [])
                let class_id_tmp = self.new_temp()
                let class_id_ty = @types.Type::Int
                let new_binders = [(class_id_tmp, class_id_ty)]
                for arg_ty in args_ty {
                  let tmp = self.new_temp()
                  new_binders.push((tmp, arg_ty))
                  bind_env2 = bind_env2.add(occur, (tmp, arg_ty))
                }
                let_list.push(Binding::LetTuple(new_binders, x.0))
                // unpack ctor arguments
                let subtreek = self.decision_tree_to_knf!(
                  subtree, let_list, bind_env2, env, ty_env,
                )
                (
                  IfEq(class_id_tmp, o, bind_all(subtreek), bind_all(switchks)),
                  subtreek.1,
                  let_list,
                )
              }
              _ => self.tracer.abort("not an enum type")
            }
          }
          Tuple(~tsize) => {
            // unpack occurrences and bind name
            let x = match bind_env.find(occur) {
              Some(x) => x
              None => fail!("unbound occurrence: \{occur}")
            }
            let new_binders = []
            for i in 0..<tsize {
              let tmp = self.new_temp()
              let dyn_ty = Type::Var(Ref::new(None))
              new_binders.push((tmp, dyn_ty))
              bind_env2 = bind_env2.add(occur, (tmp, dyn_ty))
            }
            let_list.push(Binding::LetTuple(new_binders, x.0))
            let subtreek = self.decision_tree_to_knf!(
              subtree, let_list, bind_env2, env, ty_env,
            )
            (bind_all(subtreek), subtreek.1, let_list)
          }
        }
        switchks = checkk
      }
      switchks
    }
  }
}

fn build_knf(
  self : KnfEnv,
  node : @types.Syntax,
  let_list : LetList,
  env : NameEnv,
  ty_env : LocalEnv
) -> KnfResult!Failure {
  self.tracer.println("build_knf: \{node}")
  match node {
    Unit => (Unit, Unit, let_list)
    Int(i) => (Int(i), Int, let_list)
    Neg(Int(i), ..) => (Int(-i), Int, let_list)
    Bool(b) => (Bool(b), Bool, let_list)
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
      let r1 = self.build_knf!(xs, let_list, env, ty_env)
      let tmp = self.materialize_knf(r1, let_list)
      let r2 = self.build_knf!(Bool(true), let_list, env, ty_env)
      let const_1 = self.materialize_knf(r2, let_list)
      (IfEq(tmp, const_1, Bool(false), Bool(true)), Int, let_list)
    }
    Array(len, elem) => {
      let r1 = self.build_knf!(len, let_list, env, ty_env)
      let len = self.materialize_knf(r1, let_list)
      let r2 = self.build_knf!(elem, let_list, env, ty_env)
      let elem = self.materialize_knf(r2, let_list)
      let call = match r2.1.repr() {
        Int | Bool =>
          ExternalFunctionApplication("minimbt_create_array", [len, elem])
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
          match self.externals.find(f).unwrap() {
            @types.Type::Fun(_, t) => t
            _ =>
              fail!(
                "not a function: \{fv} : \{self.externals.find(f).unwrap()}",
              )
          },
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
      let r3 = self.build_knf!(Bool(true), let_list, env, ty_env)
      let const_1 = self.materialize_knf(r3, let_list)
      (IfEq(x, const_1, bind_all(r1), bind_all(r2)), r1.1, let_list)
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
      (IfEq(x, y, Bool(true), Bool(false)), Int, let_list)
    }
    LE(o1, o2) => {
      let r1 = self.build_knf!(o1, let_list, env, ty_env)
      let x = self.materialize_knf(r1, let_list)
      let r2 = self.build_knf!(o2, let_list, env, ty_env)
      let y = self.materialize_knf(r2, let_list)
      (IfLe(x, y, Bool(true), Bool(false)), Int, let_list)
    }
    Let((x, ty), e1, e2) => {
      let r1 = self.build_knf!(e1, let_list, env, ty_env)
      let e1 = self.materialize_knf_with_name!(r1, let_list, x)
      let env = env.add(x, e1)
      let ty_env = ty_env.add(x, ty)
      let r2 = self.build_knf!(e2, let_list, env, ty_env)
      (r2.0, r2.1, let_list)
    }
    CtorApp(Var(f), xs) => {
      let ctor_ty = self.named_tys.find(f).unwrap()
      match ctor_ty {
        @types.Type::Enum(_, variants) => {
          let variant_id = variants.search_by(fn(ctor) { f == ctor.0 }).unwrap()
          let idk = self.build_knf!(
            Int(variant_id.to_int64()),
            let_list,
            env,
            ty_env,
          )
          let idn = self.materialize_knf(idk, let_list)
          let args = [idn]
          for x in xs {
            let r = self.build_knf!(x, let_list, env, ty_env)
            let tmp = self.materialize_knf(r, let_list)
            args.push(tmp)
          }
          (Tuple(args), ctor_ty, let_list)
        }
        _ => self.tracer.abort("not an enum type")
      }
    }
    Match(e, _) as matcher => {
      let r = self.build_knf!(e, let_list, env, ty_env)
      let scrutinee = self.materialize_knf(r, let_list)
      let tree = PatternMatcher::new(matcher, self.tracer).run()
      let bind_env = BindEnv::of([(Occurrence::empty(), (scrutinee, r.1))])
      let r = self.decision_tree_to_knf!(tree, let_list, bind_env, env, ty_env)
      r
    }
    LetEnum(enumty, e) => {
      let mut new_env = env
      let mut new_ty_env = ty_env
      guard let Type::Enum(_, variants) = enumty else {
        _ => fail!("check: \{enumty} is not an enum type")
        // named_tys = named_tys.add(name, enumty)
      }
      for variant in variants {
        let (cname, args_ty) = variant
        // Add constructors to type scheme environment.
        // This is for constructors such as 'None' and 'Nil'.
        // This can be generalized to generic ADTs.
        if args_ty.length() > 0 {
          let variant_ty = Type::Fun(args_ty, enumty)
          new_ty_env = new_ty_env.add(cname, variant_ty)
        } else {
          // placeholder Name for 0 arity constructors.
          let tmp = self.new_temp()
          new_env = new_env.add(cname, tmp)
          new_ty_env = new_ty_env.add(cname, enumty)
        }
      }
      self.build_knf!(e, let_list, new_env, new_ty_env)
    }
    LetRec({ name, args, body, .. }, e) => {
      let ty = name.1
      let orig_name = name.0
      let name = self.make_name(orig_name)
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
        let x_name = self.make_name(x)
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
        let x_name = self.make_name(x)
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
    _ => self.tracer.abort("syntax_to_knf: unreachable syntax")
  }
}
