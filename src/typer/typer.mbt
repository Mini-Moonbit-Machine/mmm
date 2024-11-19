typealias Syntax = @types.Syntax

typealias Pattern = @types.Pattern

typealias Type = @types.Type

typealias TypeScheme = @types.TypeScheme

typealias Kind = @types.Kind

type TyVEnv @immut/hashmap.T[String, Type]

typealias TySEnv = @immut/hashmap.T[String, TypeScheme]

pub struct Typer {
  syntax : @types.Syntax
  tracer : @util.SubTracer
}

pub fn Typer::new(syntax : Syntax, tracer : @util.SubTracer) -> Typer {
  Typer::{ syntax, tracer }
}

pub fn Typer::map_named(
  self : Typer,
  ty1 : Type,
  m : (String) -> Type,
  ~subst_enum : Bool = false // we only want to substitute enum types when resolving them
  // in other time, the enum can't contain type parameters, so it's safe to keep them as is
) -> Type {
  let _ = self
  let mem = []
  fn aux(t : Type) {
    match t {
      Var(r) =>
        match r.val {
          Some(t) => {
            for t2 in mem {
              if physical_equal(t, t2) {
                return t
              }
            }
            mem.push(t)
            let t2 = aux(t)
            Var(Ref::new(Some(t2)))
          }
          None => t
        }
      Named(name) => m(name)
      Fun(args, ret) => Type::Fun(args.map(aux), aux(ret))
      Tuple(xs) => Tuple(xs.map(aux))
      Array(x) => Array(aux(x))
      Enum(name, variants) =>
        if subst_enum {
          Enum(
            name,
            variants.map(fn(variant) { (variant.0, variant.1.map(aux)) }),
          )
        } else {
          t
        }
      _ => t
    }
  }

  aux(ty1)
}

fn Typer::replace(
  self : Typer,
  ty1 : Type,
  tenv : TyVEnv,
  ~subst_enum : Bool = false
) -> Type {
  let _ = self
  fn m(name) {
    match tenv._.find(name) {
      Some(Named(t2) as t) => if name == t2 { Type::Named(name) } else { t } // for type parameters
      Some(t) => t
      None => Type::Named(name)
    }
  }

  self.map_named(ty1, m, ~subst_enum)
}

fn Typer::unfold(self : Typer, ty1 : Type, tenv : TyVEnv) -> Type {
  self.replace(ty1, tenv).repr()
}

fn Typer::fold(self : Typer, ty1 : Type, tenv : TyVEnv) -> Type {
  self.replace(ty1, tenv)
}

fn Typer::inst_unfold(self : Typer, ts : TypeScheme, envt : TyVEnv) -> Type {
  self.tracer.println("inst: \{ts}")
  let (tyvars, ty) = ts._
  let mut env = envt._
  for t in tyvars {
    env = env.add(t, Type::new_var())
  }
  let ty = self.unfold(ty, env)
  self.tracer.println("inst result: \{ty}")
  ty
}

fn Typer::inst_fold(self : Typer, ts : TypeScheme, envt : TyVEnv) -> Type {
  self.tracer.println("inst: \{ts}")
  let (tyvars, ty) = ts._
  let mut env = envt._
  for t in tyvars {
    env = env.add(t, Type::new_var())
  }
  let ty = self.fold(ty, env)
  self.tracer.println("inst result: \{ty}")
  ty
}

fn Typer::subst(self : Typer, ty1 : Type) -> Type {
  fn aux(t : Type) {
    let repr = t.repr()
    match repr {
      Type::Var(_) => self.tracer.abort("unknown type variable")
      Fun(args, ret) => Type::Fun(args.map(aux), aux(ret))
      Tuple(xs) => Tuple(xs.map(aux))
      Array(x) => Array(aux(x))
      Enum(_, _) => t
      _ => repr
    }
  }

  aux(ty1)
}

fn Typer::resolve_enum_type(
  self : Typer,
  name : String,
  ty1 : Type,
  tenv : TyVEnv
) -> TyVEnv {
  self.tracer.println("let enum before resolved: \{ty1}")
  guard let @types.Type::Var({ val: Some(@types.Type::Enum(_)) } as enum_ty_var) = ty1 else {
    _ => self.tracer.abort("resolve_enum_type: not a type variable")
  }
  let tenv = tenv._.add(name, ty1)
  let unfolded = self.replace(ty1, tenv, subst_enum=true)
  enum_ty_var.val = Some(unfolded)
  self.tracer.println("let enum after resolved: \{ty1}")
  tenv
}

fn Typer::union(self : Typer, ty1 : Type, ty2 : Type) -> Unit!Failure {
  self.tracer.println("union: \{ty1} and \{ty2}")
  match (ty1, ty2) {
    (Type::Var(t1), Type::Var(_)) =>
      if physical_equal(ty1, ty2) {
        return
      } else {
        t1.val = Some(ty2)
      }
    (Type::Var(t), _) => t.val = Some(ty2)
    (_, Type::Var(t)) => t.val = Some(ty1)
    _ =>
      fail!("union: unable to union without a type variable: \{ty1} and \{ty2}")
  }
}

fn Typer::unify(
  self : Typer,
  ty1 : Type,
  ty2 : Type,
  tenv : TyVEnv,
  node : Syntax
) -> Unit!Failure {
  self.tracer.println("unify: \{ty1} and \{ty2}")
  let ty1 = self.unfold(ty1, tenv)
  let ty2 = self.unfold(ty2, tenv)
  match (ty1, ty2) {
    (Type::Unit, Type::Unit) => ()
    (Type::Bool, Type::Bool) => ()
    (Type::Int, Type::Int) => ()
    (Type::Double, Type::Double) => ()
    (Type::String, Type::String) => ()
    (Type::Fun(args1, ret1), Type::Fun(args2, ret2)) => {
      if args1.length() != args2.length() {
        fail!("unify: different number of arguments")
      }
      for i, x in args1 {
        self.unify!(x, args2[i], tenv, node)
      }
      self.unify!(ret1, ret2, tenv, node)
    }
    (Type::Tuple(xs), Type::Tuple(ys)) => {
      if xs.length() != ys.length() {
        fail!("unify: different number of elements")
      }
      for i, x in xs {
        self.unify!(x, ys[i], tenv, node)
      }
    }
    (Type::Array(x), Type::Array(y)) => self.unify!(x, y, tenv, node)
    (Type::Named(x), Type::Named(y)) =>
      if x != y {
        fail!("unify: different type names \{x} and \{y}")
      }
    (Enum(x, _), Enum(y, _)) =>
      if x != y {
        fail!("unify: enums \{x} and \{y}")
      }
    (Type::Var(_), _) => self.union!(ty1, ty2)
    (_, Type::Var(_)) => self.union!(ty1, ty2)
    _ =>
      fail!("unify: mismatched types \{ty1} and \{ty2}, when handling \{node}")
  }
}

fn Typer::check(
  self : Typer,
  node : Syntax,
  ty : Type,
  envt : TyVEnv,
  envs : TySEnv
) -> Unit!Failure {
  self.tracer.println("check: \{node} against \{ty}")
  let ty = self.unfold(ty, envt)
  if ty.is_unknown() {
    fail!("check \{node} against unknown type \{ty}")
  }
  match (node, ty) {
    (Syntax::Unit, Type::Unit) => ()
    (Syntax::Bool(_), Type::Bool) => ()
    (Syntax::Int(_), Type::Int) => ()
    (Syntax::Double(_), Type::Double) => ()
    (Syntax::String(_), Type::String) => ()
    (Syntax::Var(x), ty2) =>
      match envs.find(x) {
        Some(t) => {
          let ty1 = self.inst_unfold(t, envt)
          self.unify!(ty1, ty2, envt, node)
        }
        None => fail!("unbound variable: \{x}")
      }
    (Syntax::Tuple(xs), Type::Tuple(ys)) => {
      if xs.length() != ys.length() {
        fail!("check: different number of elements")
      }
      for i, x in xs {
        self.check!(x, ys[i], envt, envs)
      }
    }
    (Syntax::Not(x), Type::Bool) => self.check!(x, Type::Bool, envt, envs)
    (Syntax::Array(len, elem), Type::Array(y)) => {
      self.check!(elem, y, envt, envs)
      self.check!(len, Type::Int, envt, envs)
    }
    (Syntax::Neg(x, kind=None | Some(Kind::Int)) as r, Type::Int) => {
      self.check!(x, Type::Int, envt, envs)
      r.kind = Some(Kind::Int)
    }
    (Syntax::Neg(x, kind=None | Some(Kind::Double)) as r, Type::Double) => {
      self.check!(x, Type::Double, envt, envs)
      r.kind = Some(Kind::Double)
    }
    (Syntax::CtorApp(f, args), ty) => {
      let args_ty = []
      for arg in args {
        let arg_ty = self.infer!(arg, envt, envs)
        args_ty.push(arg_ty)
      }
      let expected_ty = Type::Fun(args_ty, ty)
      match envs.find(f) {
        Some(t) => {
          let ctor_ty = self.inst_fold(t, envt)
          self.unify!(ctor_ty, expected_ty, envt, node)
        }
        None => fail!("unbound constructor: \{f}")
      }
    }
    (Syntax::App(f, args), ty) => {
      let args_ty = []
      for arg in args {
        let arg_ty = self.infer!(arg, envt, envs)
        args_ty.push(arg_ty)
      }
      let expected_fun_ty = Type::Fun(args_ty, ty)
      self.check!(f, expected_fun_ty, envt, envs)
    }
    (Syntax::Get(arr, idx), ty) => {
      self.check!(arr, Type::Array(ty), envt, envs)
      self.check!(idx, Type::Int, envt, envs)
    }
    (Syntax::If(cond, t, f), ty) => {
      self.check!(cond, Type::Bool, envt, envs)
      self.check!(t, ty, envt, envs)
      self.check!(f, ty, envt, envs)
    }
    (Syntax::Prim(x, y, _, kind=None | Some(Kind::Int)) as r, Type::Int) => {
      self.check!(x, Type::Int, envt, envs)
      self.check!(y, Type::Int, envt, envs)
      r.kind = Some(Kind::Int)
    }
    (Syntax::Prim(x, y, _, kind=None | Some(Kind::Double)) as r, Type::Double) => {
      self.check!(x, Type::Double, envt, envs)
      self.check!(y, Type::Double, envt, envs)
      r.kind = Some(Kind::Double)
    }
    (Syntax::Eq(x, y), Type::Bool) => {
      let t1 = self.infer!(x, envt, envs)
      let t2 = self.infer!(y, envt, envs)
      self.unify!(t1, t2, envt, node)
    }
    (Syntax::LE(x, y), Type::Bool) => {
      let t1 = self.infer!(x, envt, envs)
      let t2 = self.infer!(y, envt, envs)
      self.unify!(t1, t2, envt, node)
    }
    (Syntax::LetEnum(name, ty, e), ty2) => {
      let envt = self.resolve_enum_type(name, ty, envt)
      let mut envs = envs
      guard let Type::Enum(_, variants) = ty.repr() else {
        _ => fail!("check: \{ty} is not an enum type")
      }
      for variant in variants {
        let (cname, args_ty) = variant
        // Add constructors to type scheme environment.
        // This is for constructors such as 'None' and 'Nil'.
        let variant_ty = Type::Fun(args_ty, ty)
        envs = envs.add(cname, variant_ty.to_scheme())
      }
      self.check!(e, ty2, envt, envs)
    }
    (Syntax::Let((x, t), e1, e2), ty) => {
      let t = if t.is_unknown() {
        let inferred = self.infer!(e1, envt, envs)
        self.unify!(t, inferred, envt, node)
        t
      } else {
        self.check!(e1, t, envt, envs)
        t
      }
      let envs = envs.add(x, t.to_scheme())
      self.check!(e2, ty, envt, envs)
    }
    (Syntax::LetRec({ args, name, tyvars, body }, e), ty) => {
      let guessed_ty = Type::Fun(
        args.iter().map(fn(x) { x.1 }).collect(),
        name.1,
      )
      let guessed_ty = (tyvars, guessed_ty)
      let envs = envs.add(name.0, guessed_ty)
      let old_envs = envs
      let old_envt = envt
      let mut envs = envs
      let mut envt = envt
      self.tracer.println("typevars: \{tyvars}")
      for t in tyvars {
        let tv = Type::new_named(t)
        envt = envt._.add(t, tv)
      }
      for xt in args {
        envs = envs.add(xt.0, xt.1.to_scheme())
      }
      if name.1.is_unknown() {
        let t = self.infer!(body, envt, envs)
        self.unify!(t, name.1, envt, node)
      } else {
        self.check!(body, name.1, envt, envs)
      }
      self.check!(e, ty, old_envt, old_envs)
    }
    (Syntax::LetTuple(xts, e1, e2), t2) =>
      if xts.iter().all(fn(xt) { xt.1.is_unknown() == false }) {
        let t1 = Type::Tuple(xts.iter().map(fn(xt) { xt.1 }).collect())
        self.check!(e1, t1, envt, envs)
        let mut envs = envs
        for xt in xts {
          envs = envs.add(xt.0, self.fold(xt.1, envt).to_scheme())
        }
        self.check!(e2, t2, envt, envs)
      } else {
        let t1 = self.infer!(e1, envt, envs)
        match t1 {
          Type::Tuple(ts) => {
            if xts.length() != ts.length() {
              fail!("check: different number of elements")
            }
            for i, xt in xts {
              match xt {
                (_, t) => self.unify!(t, ts[i], envt, node)
              }
            }
            let mut envs = envs
            for i, xt in xts {
              envs = envs.add(xt.0, self.fold(ts[i], envt).to_scheme())
            }
            self.check!(e2, t2, envt, envs)
          }
          _ => fail!("check: not a tuple")
        }
      }
    (Syntax::Put(arr, idx, x), Type::Unit) => {
      self.check!(arr, Type::Array(ty), envt, envs)
      self.check!(idx, Type::Int, envt, envs)
      self.check!(x, ty, envt, envs)
    }
    (Syntax::Match(e, arms), ty) => {
      let et = self.infer!(e, envt, envs)
      for arm in arms {
        let pattern = arm.pat
        let action = arm.action
        let arm_envs = self.check_and_bind_pattern!(
          pattern, et, action, envt, envs,
        )
        self.check!(action, ty, envt, arm_envs)
      }
    }
    _ => fail!("check: mismatched types \{node} and \{ty}")
  }
}

fn Typer::check_and_bind_pattern(
  self : Typer,
  pattern : Pattern,
  pt : Type,
  action : Syntax,
  envt : TyVEnv,
  envs : TySEnv
) -> TySEnv!Failure {
  match pattern {
    Pattern::Wildcard => envs
    Unit => {
      self.unify!(pt, Type::Unit, envt, action)
      envs
    }
    Bool(_) => {
      self.unify!(pt, Type::Bool, envt, action)
      envs
    }
    Int(_) => {
      self.unify!(pt, Type::Int, envt, action)
      envs
    }
    Double(_) => {
      self.unify!(pt, Type::Double, envt, action)
      envs
    }
    String(_) => {
      self.unify!(pt, Type::String, envt, action)
      envs
    }
    Var(x) => {
      let envs = envs.add(x, pt.to_scheme())
      envs
    }
    Tuple(ps) =>
      match pt {
        Type::Tuple(ts) => {
          if ps.length() != ts.length() {
            fail!("check_pattern: different number of elements")
          }
          let mut new_envs = envs
          for i, p in ps {
            new_envs = self.check_and_bind_pattern!(
              p,
              ts[i],
              action,
              envt,
              new_envs,
            )
          }
          new_envs
        }
        _ => fail!("check pattern: \{pattern} not a tuple")
      }
    CtorApp(name, pats) => {
      let ty = match envs.find(name) {
        Some(t) => self.inst_fold(t, envt)
        None => fail!("unbound constructor: \{name}")
      }
      match ty {
        Type::Fun(args_ty, ret_ty) => {
          if pats.length() != args_ty.length() {
            fail!("check_pattern: different number of arguments")
          }
          self.unify!(pt, ret_ty, envt, action)
          let mut new_envs = envs
          for i, p in pats {
            new_envs = self.check_and_bind_pattern!(
              p,
              args_ty[i],
              action,
              envt,
              new_envs,
            )
          }
          new_envs
        }
        _ => fail!("check pattern: \{ty} not a function type")
      }
    }
  }
}

fn Typer::infer(
  self : Typer,
  node : Syntax,
  envt : TyVEnv,
  envs : TySEnv
) -> Type!Failure {
  self.tracer.println("infer: \{node}")
  match node {
    Syntax::Unit => Type::Unit
    Syntax::Bool(_) => Type::Bool
    Syntax::Int(_) => Type::Int
    Syntax::Double(_) => Type::Double
    Syntax::String(_) => Type::String
    Syntax::Var(x) =>
      match envs.find(x) {
        Some(t) => self.inst_unfold(t, envt)
        None => fail!("unbound variable: \{x}")
      }
    Syntax::Tuple(xs) => {
      let ts = []
      for x in xs {
        ts.push(self.infer!(x, envt, envs))
      }
      Type::Tuple(ts)
    }
    Syntax::Not(x) => {
      self.check!(x, Type::Bool, envt, envs)
      Type::Bool
    }
    Syntax::Array(len, elem) => {
      let elem_ty = self.infer!(elem, envt, envs)
      self.check!(len, Type::Int, envt, envs)
      Type::Array(elem_ty)
    }
    Syntax::Neg(x, kind=None) as r => {
      let ty = self.infer!(x, envt, envs)
      let ty = match ty {
        Type::Int => {
          r.kind = Some(Kind::Int)
          Type::Int
        }
        Type::Double => {
          r.kind = Some(Kind::Double)
          Type::Double
        }
        _ => fail!("infer: invalid type for negation: \{ty}")
      }
      self.unfold(ty, envt)
    }
    Syntax::Neg(x, kind=Some(Kind::Int)) => {
      self.check!(x, Type::Int, envt, envs)
      Type::Int
    }
    Syntax::Neg(x, kind=Some(Kind::Double)) => {
      self.check!(x, Type::Double, envt, envs)
      Type::Double
    }
    Syntax::CtorApp(f, args) => {
      let inferred_fun_ty = match envs.find(f) {
        Some(t) => self.inst_unfold(t, envt)
        None => fail!("unbound constructor: \{f}")
      }
      match inferred_fun_ty {
        Type::Fun(args_ty, ret_ty) => {
          if args.length() != args_ty.length() {
            fail!("infer: different number of arguments")
          }
          for i, arg in args {
            if args_ty[i].is_unknown() {
              let arg_ty = self.infer!(arg, envt, envs)
              self.unify!(args_ty[i], arg_ty, envt, node)
            } else {
              self.check!(arg, args_ty[i], envt, envs)
            }
          }
          self.fold(ret_ty, envt)
        }
        _ =>
          fail!(
            "infer: constructor not a known function type: \{inferred_fun_ty}",
          )
      }
    }
    Syntax::App(f, args) => {
      let inferred_fun_ty = self.infer!(f, envt, envs)
      match inferred_fun_ty {
        Type::Fun(args_ty, ret_ty) => {
          if args.length() != args_ty.length() {
            fail!("infer: different number of arguments")
          }
          for i, arg in args {
            if args_ty[i].is_unknown() {
              let arg_ty = self.infer!(arg, envt, envs)
              self.unify!(args_ty[i], arg_ty, envt, node)
            } else {
              self.check!(arg, args_ty[i], envt, envs)
            }
          }
          self.unfold(ret_ty, envt)
        }
        Type::Var({ val: None }) => {
          let guessed_ret_ty = Type::new_var()
          let args_ty = []
          for arg in args {
            let arg_ty = self.infer!(arg, envt, envs)
            args_ty.push(arg_ty)
          }
          let guessed_fun_ty = Type::Fun(args_ty, guessed_ret_ty)
          self.unify!(inferred_fun_ty, guessed_fun_ty, envt, node)
          self.unfold(guessed_ret_ty, envt)
        }
        _ => fail!("infer: not a function type: \{inferred_fun_ty}")
      }
    }
    Syntax::Get(arr, idx) => {
      let arr_ty = self.infer!(arr, envt, envs)
      match arr_ty {
        Type::Array(ty) => {
          self.check!(idx, Type::Int, envt, envs)
          self.unfold(ty, envt)
        }
        _ => fail!("infer: not an array")
      }
    }
    Syntax::If(cond, t, f) => {
      self.check!(cond, Type::Bool, envt, envs)
      let ty1 = self.infer!(t, envt, envs)
      let ty2 = self.infer!(f, envt, envs)
      self.unify!(ty1, ty2, envt, node)
      self.unfold(ty1, envt)
    }
    Syntax::Prim(x, y, _, kind=None) as r => {
      let ty1 = self.infer!(x, envt, envs)
      let ty2 = self.infer!(y, envt, envs)
      self.unify!(ty1, ty2, envt, node)
      let ty1 = self.unfold(ty1, envt)
      match ty1 {
        Type::Int => {
          r.kind = Some(Kind::Int)
          Type::Int
        }
        Type::Double => {
          r.kind = Some(Kind::Double)
          Type::Double
        }
        _ => fail!("infer: invalid type for prim: \{ty1}")
      }
    }
    Syntax::Prim(x, y, _, kind=Some(Kind::Int)) => {
      self.check!(x, Type::Int, envt, envs)
      self.check!(y, Type::Int, envt, envs)
      Type::Int
    }
    Syntax::Prim(x, y, _, kind=Some(Kind::Double)) => {
      self.check!(x, Type::Double, envt, envs)
      self.check!(y, Type::Double, envt, envs)
      Type::Double
    }
    Syntax::Eq(x, y) => {
      let ty1 = self.infer!(x, envt, envs)
      if ty1.is_unknown() {
        let ty2 = self.infer!(y, envt, envs)
        self.unify!(ty1, ty2, envt, node)
      } else {
        self.check!(y, ty1, envt, envs)
      }
      Type::Bool
    }
    Syntax::LE(x, y) => {
      let ty1 = self.infer!(x, envt, envs)
      if ty1.is_unknown() {
        let ty2 = self.infer!(y, envt, envs)
        self.unify!(ty1, ty2, envt, node)
      } else {
        self.check!(y, ty1, envt, envs)
      }
      Type::Bool
    }
    Syntax::Let((x, t), e1, e2) => {
      let t = if t.is_unknown() {
        let inferred = self.infer!(e1, envt, envs)
        self.unify!(t, inferred, envt, node)
        t
      } else {
        self.check!(e1, t, envt, envs)
        t
      }
      let envs = envs.add(x, t.to_scheme())
      self.infer!(e2, envt, envs)
    }
    Syntax::LetEnum(name, ty, e) => {
      let envt = self.resolve_enum_type(name, ty, envt)
      let mut envs = envs
      guard let Type::Enum(_, variants) = ty.repr() else {
        _ => fail!("check: \{ty} is not an enum type")
      }
      for variant in variants {
        let (cname, args_ty) = variant
        // Add constructors to type scheme environment.
        // This is for constructors such as 'None' and 'Nil'.
        let variant_ty = Type::Fun(args_ty, ty)
        envs = envs.add(cname, variant_ty.to_scheme())
      }
      self.infer!(e, envt, envs)
    }
    Syntax::LetRec({ args, name, tyvars, body }, e) => {
      let guessed_ty = Type::Fun(
        args.iter().map(fn(x) { x.1 }).collect(),
        name.1,
      )
      let guessed_ty = (tyvars, guessed_ty)
      let envs = envs.add(name.0, guessed_ty)
      let old_envs = envs
      let old_envt = envt
      let mut envs = envs
      let mut envt = envt
      self.tracer.println("typevars: \{tyvars}")
      for t in tyvars {
        let tv = Type::new_named(t)
        envt = envt._.add(t, tv)
      }
      for xt in args {
        envs = envs.add(xt.0, xt.1.to_scheme())
      }
      if name.1.is_unknown() {
        let t = self.infer!(body, envt, envs)
        self.unify!(t, name.1, envt, node)
      } else {
        self.check!(body, name.1, envt, envs)
      }
      self.infer!(e, old_envt, old_envs)
    }
    Syntax::LetTuple(xts, e1, e2) => {
      let t1 = self.infer!(e1, envt, envs)
      match t1 {
        Type::Tuple(ts) => {
          if xts.length() != ts.length() {
            fail!("infer: different number of elements")
          }
          for i, xt in xts {
            match xt {
              (_, t) => self.unify!(t, ts[i], envt, node)
            }
          }
          let mut envs = envs
          for i, xt in xts {
            envs = envs.add(xt.0, self.unfold(ts[i], envt).to_scheme())
          }
          self.infer!(e2, envt, envs)
        }
        _ => fail!("infer: not a tuple")
      }
    }
    Syntax::Put(arr, idx, x) => {
      self.check!(arr, Type::Array(self.infer!(x, envt, envs)), envt, envs)
      self.check!(idx, Type::Int, envt, envs)
      Type::Unit
    }
    Syntax::Match(e, arms) => {
      let et = self.infer!(e, envt, envs)
      let actions_ty = []
      let n = arms.length()
      for arm in arms {
        let pattern = arm.pat
        let action = arm.action
        let arm_envs = self.check_and_bind_pattern!(
          pattern, et, action, envt, envs,
        )
        let action_ty = self.infer!(action, envt, arm_envs)
        actions_ty.push(action_ty)
      }
      // ring unify
      for i in 1..<n {
        self.unify!(actions_ty[i - 1], actions_ty[i], envt, node)
      }
      self.unify!(actions_ty[n - 1], actions_ty[0], envt, node)
      self.fold(actions_ty[0], envt)
    }
  }
}

pub fn Typer::simplify(self : Typer, syntax : Syntax) -> Syntax {
  let _ = self
  fn aux(syn : Syntax) {
    match syn {
      Tuple(xs) => Syntax::Tuple(xs.map(aux))
      Not(x) => Not(aux(x))
      Array(len, elem) => Array(aux(len), aux(elem))
      Neg(x, ~kind) => Neg(aux(x), ~kind)
      App(f, args) => App(aux(f), args.map(aux))
      Get(arr, idx) => Get(aux(arr), aux(idx))
      If(cond, t, f) => If(aux(cond), aux(t), aux(f))
      Prim(x, y, op, ~kind) => Prim(aux(x), aux(y), op, ~kind)
      Eq(x, y) => Eq(aux(x), aux(y))
      LE(x, y) => LE(aux(x), aux(y))
      Let((x, t), e1, e2) => Let((x, self.subst(t)), aux(e1), aux(e2))
      LetRec({ args, name, tyvars, body }, e) => {
        let args = args.map(fn(xt) { (xt.0, self.subst(xt.1)) })
        let name = (name.0, self.subst(name.1))
        LetRec({ args, name, tyvars, body: aux(body) }, aux(e))
      }
      LetTuple(xts, e1, e2) => {
        let xts = xts.map(fn(xt) { (xt.0, self.subst(xt.1)) })
        LetTuple(xts, aux(e1), aux(e2))
      }
      LetEnum(name, ty, e) => LetEnum(name, ty, aux(e))
      Put(arr, idx, x) => Put(aux(arr), aux(idx), aux(x))
      _ => syn
    }
  }

  aux(syntax)
}

pub fn externals() -> @immut/hashmap.T[String, @types.Type] {
  @immut/hashmap.T::new()
  .add("read_int", @types.Fun([], Int))
  .add("read_char", @types.Fun([], Int))
  .add("print_int", @types.Fun([Int], Unit))
  .add("print_char", @types.Fun([Int], Unit))
  .add("print_endline", @types.Fun([], Unit))
  .add("print_newline", @types.Fun([], Unit))
  .add("create_array", @types.Fun([Int, Int], @types.Type::Array(Int)))
  .add(
    "create_float_array",
    @types.Fun([Int, Double], @types.Type::Array(Double)),
  )
  .add("create_ptr_array", @types.Fun([Int, Ptr], @types.Type::Array(Ptr)))
  .add("truncate", @types.Fun([Double], Int))
  .add("floor", @types.Fun([Double], Double))
  .add("sin", @types.Fun([Double], Double))
  .add("cos", @types.Fun([Double], Double))
  .add("atan", @types.Fun([Double], Double))
  .add("sqrt", @types.Fun([Double], Double))
  .add("abs_float", @types.Fun([Double], Double))
  .add("int_of_float", @types.Fun([Double], Int))
  .add("float_of_int", @types.Fun([Int], Double))
}

pub fn Typer::check_all(self : Typer) -> Syntax!Failure {
  let mut envs = @immut/hashmap.T::new()
  for ext in externals() {
    envs = envs.add(ext.0, ext.1.to_scheme())
  }
  self.check!(self.syntax, Type::Unit, @immut/hashmap.T::new(), envs)
  self.tracer.record("typer", self.syntax.to_pretty_print())
  let syn = self.simplify(self.syntax)
  self.tracer.record("typer_simplified", syn.to_pretty_print())
  syn
}
