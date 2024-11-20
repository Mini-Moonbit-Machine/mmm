typealias Syntax = @types.Syntax

typealias Pattern = @types.Pattern

typealias Type = @types.Type

typealias TypeScheme = @types.TypeScheme

typealias Kind = @types.Kind

type TyVEnv @immut/hashmap.T[String, Type]

struct TyCEnv {
  classes : @immut/hashmap.T[String, @types.Classdef]
  methods_of_classes : @immut/hashmap.T[String, String]
  methods_type : @immut/hashmap.T[String, Type]
}

fn TyCEnv::new() -> TyCEnv {
  TyCEnv::{
    classes: @immut/hashmap.new(),
    methods_of_classes: @immut/hashmap.new(),
    methods_type: @immut/hashmap.new(),
  }
}

fn TyCEnv::add_class(
  self : TyCEnv,
  name : String,
  classdef : @types.Classdef
) -> TyCEnv {
  TyCEnv::{ ..self, classes: self.classes.add(name, classdef) }
}

fn TyCEnv::add_method(
  self : TyCEnv,
  name : String,
  class_name : String,
  ty : Type
) -> TyCEnv!Failure {
  if self.methods_of_classes.find(name).is_empty() == false {
    fail!("add_method: method \{name} already exists")
  }
  TyCEnv::{
    ..self,
    methods_of_classes: self.methods_of_classes.add(name, class_name),
    methods_type: self.methods_type.add(name, ty),
  }
}

fn TyCEnv::find_class(self : TyCEnv, name : String) -> @types.Classdef? {
  self.classes.find(name)
}

fn TyCEnv::find_method(self : TyCEnv, name : String) -> String? {
  self.methods_of_classes.find(name)
}

typealias TySEnv = @immut/hashmap.T[String, TypeScheme]

pub struct Typer {
  syntax : @types.Syntax
  tracer : @util.SubTracer
}

pub fn Typer::new(syntax : Syntax, tracer : @util.SubTracer) -> Typer {
  Typer::{ syntax, tracer }
}

pub fn Typer::map_named(self : Typer, ty1 : Type, m : (String) -> Type) -> Type {
  let _ = self
  let mem = @types.TypeMemSet::new()
  fn aux(rid : Int?, t : Type) {
    match rid {
      Some(_) => return t
      None => ()
    }
    match t {
      Type::Named(name) => m(name)
      Enum(_, { val } as r) => {
        let inner = val.map(
          fn(arr) {
            arr.map(
              fn(variant) {
                (
                  variant.0,
                  variant.1.map(
                    fn(x) { mem.map(x, fn(rid, x) { aux(rid, x) }) },
                  ),
                )
              },
            )
          },
        )
        r.val = inner
        t
      }
      Fun(args, ret) =>
        Fun(
          args.map(fn(x) { mem.map(x, fn(r, x) { aux(r, x) }) }),
          mem.map(ret, fn(r, x) { aux(r, x) }),
        )
      Tuple(xs) => Tuple(xs.map(fn(x) { mem.map(x, fn(r, x) { aux(r, x) }) }))
      Array(x) => Array(mem.map(x, fn(r, x) { aux(r, x) }))
      Var({ val: Some(t2) } as r) => {
        let new_val = mem.map(t2, fn(r, x) { aux(r, x) })
        r.val = Some(new_val)
        t
      }
      Var(_) => t
      Bool | Int | Double | String | Unit | Ptr => t
    }
  }

  mem.map(ty1, aux)
}

fn Typer::replace(self : Typer, ty1 : Type, tenv : TyVEnv) -> Type {
  let _ = self
  fn m(name) {
    match tenv._.find(name) {
      Some(Named(t2) as t) => if name == t2 { Type::Named(name) } else { t } // for type parameters
      Some(t) => t
      None => Type::Named(name)
    }
  }

  self.map_named(ty1, m).repr()
}

fn Typer::inst(self : Typer, ts : TypeScheme, envt : TyVEnv) -> Type {
  self.tracer.println("inst: \{ts}")
  let (tyvars, ty) = ts._
  let mut env = envt._
  for t in tyvars {
    env = env.add(t, Type::new_var())
  }
  let ty = self.replace(ty, env)
  self.tracer.println("inst result: \{ty}")
  ty
}

fn Typer::inst_and_tvars(
  self : Typer,
  ts : TypeScheme,
  envt : TyVEnv
) -> (Type, Array[(String, Type)]) {
  self.tracer.println("inst: \{ts}")
  let (tyvars, ty) = ts._
  let mut env = envt._
  let tvars = []
  for t in tyvars {
    let tv = Type::new_var()
    env = env.add(t, tv)
    tvars.push((t, tv))
  }
  let ty = self.replace(ty, env)
  self.tracer.println("inst result: \{ty}")
  (ty, tvars)
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

fn Typer::add_enum(
  self : Typer,
  name : String,
  ty1 : Type,
  tenv : TyVEnv
) -> TyVEnv {
  let _ = self
  let tenv = tenv._.add(name, ty1)
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
  let ty1 = self.replace(ty1, tenv)
  let ty2 = self.replace(ty2, tenv)
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
  envc : TyCEnv,
  envt : TyVEnv,
  envs : TySEnv
) -> Unit!Failure {
  self.tracer.println("check: \{node} against \{ty}")
  let ty = self.replace(ty, envt)
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
          let ty1 = self.inst(t, envt)
          self.unify!(ty1, ty2, envt, node)
        }
        None => fail!("unbound variable: \{x}")
      }
    (Syntax::Tuple(xs), Type::Tuple(ys)) => {
      if xs.length() != ys.length() {
        fail!("check: different number of elements")
      }
      for i, x in xs {
        self.check!(x, ys[i], envc, envt, envs)
      }
    }
    (Syntax::Not(x), Type::Bool) => self.check!(x, Type::Bool, envc, envt, envs)
    (Syntax::Array(len, elem), Type::Array(y)) => {
      self.check!(elem, y, envc, envt, envs)
      self.check!(len, Type::Int, envc, envt, envs)
    }
    (Syntax::Neg(x, kind=None | Some(Kind::Int)) as r, Type::Int) => {
      self.check!(x, Type::Int, envc, envt, envs)
      r.kind = Some(Kind::Int)
    }
    (Syntax::Neg(x, kind=None | Some(Kind::Double)) as r, Type::Double) => {
      self.check!(x, Type::Double, envc, envt, envs)
      r.kind = Some(Kind::Double)
    }
    (Syntax::CtorApp(f, args), ty) => {
      let args_ty = []
      for arg in args {
        let arg_ty = self.infer!(arg, envc, envt, envs)
        args_ty.push(arg_ty)
      }
      let expected_ty = Type::Fun(args_ty, ty)
      match envs.find(f) {
        Some(t) => {
          let ctor_ty = self.inst(t, envt)
          self.unify!(ctor_ty, expected_ty, envt, node)
        }
        None => fail!("unbound constructor: \{f}")
      }
    }
    (Syntax::App(Var(f), targs=_, args) as app, ty) => {
      let args_ty = []
      for arg in args {
        let arg_ty = self.infer!(arg, envc, envt, envs)
        args_ty.push(arg_ty)
      }
      let expected_fun_ty = Type::Fun(args_ty, ty)
      let tvars = match envs.find(f) {
        Some(t) => {
          let (ty1, tvars) = self.inst_and_tvars(t, envt)
          self.unify!(ty1, expected_fun_ty, envt, node)
          tvars
        }
        None => fail!("unbound variable: \{f}")
      }
      app.targs = Some(tvars)
    }
    (Syntax::App(f, targs=_, args), ty) => {
      let args_ty = []
      for arg in args {
        let arg_ty = self.infer!(arg, envc, envt, envs)
        args_ty.push(arg_ty)
      }
      let expected_fun_ty = Type::Fun(args_ty, ty)
      self.check!(f, expected_fun_ty, envc, envt, envs)
    }
    (Syntax::Get(arr, idx), ty) => {
      self.check!(arr, Type::Array(ty), envc, envt, envs)
      self.check!(idx, Type::Int, envc, envt, envs)
    }
    (Syntax::If(cond, t, f), ty) => {
      self.check!(cond, Type::Bool, envc, envt, envs)
      self.check!(t, ty, envc, envt, envs)
      self.check!(f, ty, envc, envt, envs)
    }
    (Syntax::Prim(x, y, _, kind=None | Some(Kind::Int)) as r, Type::Int) => {
      self.check!(x, Type::Int, envc, envt, envs)
      self.check!(y, Type::Int, envc, envt, envs)
      r.kind = Some(Kind::Int)
    }
    (Syntax::Prim(x, y, _, kind=None | Some(Kind::Double)) as r, Type::Double) => {
      self.check!(x, Type::Double, envc, envt, envs)
      self.check!(y, Type::Double, envc, envt, envs)
      r.kind = Some(Kind::Double)
    }
    (Syntax::Eq(x, y), Type::Bool) => {
      let t1 = self.infer!(x, envc, envt, envs)
      let t2 = self.infer!(y, envc, envt, envs)
      self.unify!(t1, t2, envt, node)
    }
    (Syntax::LE(x, y), Type::Bool) => {
      let t1 = self.infer!(x, envc, envt, envs)
      let t2 = self.infer!(y, envc, envt, envs)
      self.unify!(t1, t2, envt, node)
    }
    (Syntax::LetEnum(_, ty, e), ty2) => {
      let mut envs = envs
      guard let Type::Enum(_, { val: Some(variants) }) = ty.repr() else {
        _ => fail!("check: \{ty} is not an enum type")
      }
      for variant in variants {
        let (cname, args_ty) = variant
        // Add constructors to type scheme environment.
        // This is for constructors such as 'None' and 'Nil'.
        let variant_ty = Type::Fun(args_ty, ty)
        envs = envs.add(cname, variant_ty.to_scheme())
      }
      self.check!(e, ty2, envc, envt, envs)
    }
    (Syntax::Let((x, t), e1, e2), ty) => {
      let t = if t.is_unknown() {
        let inferred = self.infer!(e1, envc, envt, envs)
        self.unify!(t, inferred, envt, node)
        t
      } else {
        self.check!(e1, t, envc, envt, envs)
        t
      }
      let envs = envs.add(x, t.to_scheme())
      self.check!(e2, ty, envc, envt, envs)
    }
    (Syntax::LetRec(fundef, e), ty) => {
      let envs = self.let_rec_ty(fundef, envs)
      self.let_rec!(fundef, envc, envt, envs)
      self.check!(e, ty, envc, envt, envs)
    }
    (Syntax::LetTuple(xts, e1, e2), t2) =>
      if xts.iter().all(fn(xt) { xt.1.is_unknown() == false }) {
        let t1 = Type::Tuple(xts.iter().map(fn(xt) { xt.1 }).collect())
        self.check!(e1, t1, envc, envt, envs)
        let mut envs = envs
        for xt in xts {
          envs = envs.add(xt.0, self.replace(xt.1, envt).to_scheme())
        }
        self.check!(e2, t2, envc, envt, envs)
      } else {
        let t1 = self.infer!(e1, envc, envt, envs)
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
              envs = envs.add(xt.0, self.replace(ts[i], envt).to_scheme())
            }
            self.check!(e2, t2, envc, envt, envs)
          }
          _ => fail!("check: not a tuple")
        }
      }
    (Syntax::Put(arr, idx, x), Type::Unit) => {
      self.check!(arr, Type::Array(ty), envc, envt, envs)
      self.check!(idx, Type::Int, envc, envt, envs)
      self.check!(x, ty, envc, envt, envs)
    }
    (Syntax::Match(e, arms), ty) => {
      let et = self.infer!(e, envc, envt, envs)
      for arm in arms {
        let pattern = arm.pat
        let action = arm.action
        let arm_envs = self.check_and_bind_pattern!(
          pattern, et, action, envt, envs,
        )
        self.check!(action, ty, envc, envt, arm_envs)
      }
    }
    (Syntax::LetClass(classdef, e), ty) => {
      let mut envc = envc.add_class(classdef.name, classdef)
      for mtd in classdef.methods {
        let (name, ty) = mtd
        envc = envc.add_method!(name, classdef.name, ty)
      }
      self.check!(e, ty, envc, envt, envs)
    }
    (Syntax::LetInstance({ ty, name, methods }, e), ty2) => {
      let cls = match envc.find_class(name) {
        Some(cls) => cls
        None => fail!("infer: unknown class \{name}")
      }
      let inside_envt = envt._.add(cls.tyvar, ty)
      let implemented = @hashset.new()
      for method in methods {
        let (name, body) = method
        let method_ty = match cls.methods.search_by(fn(m) { m.0 == name }) {
          Some(idx) => cls.methods[idx].1
          None => fail!("infer: unknown method \{name}")
        }
        let method_ty = self.replace(method_ty, inside_envt)
        self.check!(body, method_ty, envc, inside_envt, envs)
        implemented.insert(name)
      }
      for method in cls.methods {
        let (name, _) = method
        if implemented.contains(name) == false {
          fail!("infer: method \{name} not implemented")
        }
      }
      self.check!(e, ty2, envc, envt, envs)
    }
    _ => fail!("check: mismatched types \{node} and \{ty}")
  }
}

fn Typer::let_rec_ty(
  self : Typer,
  fundef : @types.Fundef,
  envs : TySEnv
) -> TySEnv {
  let _ = self
  let { args, name, tyvars, .. } = fundef
  let guessed_ty = Type::Fun(args.iter().map(fn(x) { x.1 }).collect(), name.1)
  let guessed_ty = (tyvars.map(fn(t) { t.0 }), guessed_ty)
  let envs = envs.add(name.0, guessed_ty)
  envs
}

fn Typer::let_rec(
  self : Typer,
  fundef : @types.Fundef,
  envc : TyCEnv,
  envt : TyVEnv,
  envs : TySEnv
) -> Unit!Failure {
  let { args, name, tyvars, body } = fundef
  let mut envs = envs
  let mut envt = envt
  self.tracer.println("typevars: \{tyvars}")
  for t in tyvars {
    match t.1 {
      Some(tc) =>
        match envc.find_class(tc) {
          Some(cls) => {
            let tv = Type::new_var()
            self.unify!(tv, Type::Named(t.0), envt, body)
            envt = envt._.add(cls.tyvar, tv)
            let methods_types = []
            // inst methods
            for mtd in cls.methods {
              methods_types.push((mtd.0, self.replace(mtd.1, envt)))
            }
            for mtd in methods_types {
              let (name, ty) = mtd
              envs = envs.add(name, ty.to_scheme())
            }
          }
          None => fail!("let rec: unknown class \{tc} for type var \{t.0}")
        }
      None => ()
    }
  }
  for t in tyvars {
    let tv = Type::new_named(t.0)
    envt = envt._.add(t.0, tv)
  }
  for xt in args {
    envs = envs.add(xt.0, xt.1.to_scheme())
  }
  if name.1.is_unknown() {
    let t = self.infer!(body, envc, envt, envs)
    self.unify!(t, name.1, envt, body)
  } else {
    self.check!(body, name.1, envc, envt, envs)
  }
}

fn Typer::resolve(self : Typer, node : Syntax, envt : TyVEnv) -> Syntax!Failure {
  self.tracer.println("resolve: \{node}")
  fn f(ty, envt : TyVEnv) {
    self.map_named(
      ty,
      fn(name) {
        match envt._.find(name) {
          Some(t) => t
          None => Type::Named(name)
        }
      },
    )
  }

  match node {
    Syntax::Unit
    | Syntax::Bool(_)
    | Syntax::Int(_)
    | Syntax::Double(_) | Syntax::String(_) | Syntax::Var(_) | Syntax::Tuple(_) =>
      node
    Syntax::Not(x) => Not(self.resolve!(x, envt))
    Syntax::Array(len, elem) =>
      Array(self.resolve!(len, envt), self.resolve!(elem, envt))
    Syntax::Neg(x, ~kind) => Neg(self.resolve!(x, envt), ~kind)
    Syntax::CtorApp(f, args) => {
      let args2 = []
      for arg in args {
        args2.push(self.resolve!(arg, envt))
      }
      CtorApp(f, args2)
    }
    Syntax::App(f, ~targs, args) => {
      let args2 = []
      for arg in args {
        args2.push(self.resolve!(arg, envt))
      }
      App(self.resolve!(f, envt), ~targs, args2)
    }
    Syntax::Get(arr, idx) =>
      Get(self.resolve!(arr, envt), self.resolve!(idx, envt))
    Syntax::If(cond, t, f) =>
      If(
        self.resolve!(cond, envt),
        self.resolve!(t, envt),
        self.resolve!(f, envt),
      )
    Syntax::Prim(x, y, op, ~kind) =>
      Prim(self.resolve!(x, envt), self.resolve!(y, envt), op, ~kind)
    Syntax::Eq(x, y) => Eq(self.resolve!(x, envt), self.resolve!(y, envt))
    Syntax::LE(x, y) => LE(self.resolve!(x, envt), self.resolve!(y, envt))
    Syntax::Let((x, t), e1, e2) => {
      let t2 = f(t, envt)
      Let((x, t2), self.resolve!(e1, envt), self.resolve!(e2, envt))
    }
    Syntax::LetEnum(name, ty, e) => {
      let envt = self.add_enum(name, ty, envt)
      let ty2 = f(ty, envt)
      let envt = self.add_enum(name, ty2, envt)
      LetEnum(name, ty2, self.resolve!(e, envt))
    }
    Syntax::LetRec({ args, name, tyvars, body }, e) => {
      let name = (name.0, f(name.1, envt))
      let args = args.map(fn(xt) { (xt.0, f(xt.1, envt)) })
      let body = self.resolve!(body, envt)
      LetRec({ args, name, tyvars, body }, self.resolve!(e, envt))
    }
    Syntax::LetTuple(xts, e1, e2) => {
      let xts = xts.map(fn(xt) { (xt.0, f(xt.1, envt)) })
      LetTuple(xts, self.resolve!(e1, envt), self.resolve!(e2, envt))
    }
    Syntax::Put(arr, idx, x) =>
      Put(
        self.resolve!(arr, envt),
        self.resolve!(idx, envt),
        self.resolve!(x, envt),
      )
    Syntax::Match(e, arms) => {
      let e2 = self.resolve!(e, envt)
      let arms2 = []
      for arm in arms {
        let action = self.resolve!(arm.action, envt)
        arms2.push({ ..arm, action, })
      }
      Match(e2, arms2)
    }
    Syntax::LetClass({ tyvar, name, methods }, e) => {
      let methods = methods.map(
        fn(method) {
          let (name, ty) = method
          (name, f(ty, envt))
        },
      )
      LetClass({ tyvar, name, methods }, self.resolve!(e, envt))
    }
    Syntax::LetInstance({ ty, name, methods }, e) => {
      let ty = f(ty, envt)
      let methods2 = []
      for method in methods {
        let (name, body) = method
        methods2.push((name, self.resolve!(body, envt)))
      }
      LetInstance({ ty, name, methods }, self.resolve!(e, envt))
    }
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
        Some(t) => self.inst(t, envt)
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
  envc : TyCEnv,
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
        Some(t) => self.inst(t, envt)
        None => fail!("unbound variable: \{x}")
      }
    Syntax::Tuple(xs) => {
      let ts = []
      for x in xs {
        ts.push(self.infer!(x, envc, envt, envs))
      }
      Type::Tuple(ts)
    }
    Syntax::Not(x) => {
      self.check!(x, Type::Bool, envc, envt, envs)
      Type::Bool
    }
    Syntax::Array(len, elem) => {
      let elem_ty = self.infer!(elem, envc, envt, envs)
      self.check!(len, Type::Int, envc, envt, envs)
      Type::Array(elem_ty)
    }
    Syntax::Neg(x, kind=None) as r => {
      let ty = self.infer!(x, envc, envt, envs)
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
      self.replace(ty, envt)
    }
    Syntax::Neg(x, kind=Some(Kind::Int)) => {
      self.check!(x, Type::Int, envc, envt, envs)
      Type::Int
    }
    Syntax::Neg(x, kind=Some(Kind::Double)) => {
      self.check!(x, Type::Double, envc, envt, envs)
      Type::Double
    }
    Syntax::CtorApp(f, args) => {
      let inferred_fun_ty = match envs.find(f) {
        Some(t) => self.inst(t, envt)
        None => fail!("unbound constructor: \{f}")
      }
      match inferred_fun_ty {
        Type::Fun(args_ty, ret_ty) => {
          if args.length() != args_ty.length() {
            fail!("infer: different number of arguments")
          }
          for i, arg in args {
            if args_ty[i].is_unknown() {
              let arg_ty = self.infer!(arg, envc, envt, envs)
              self.unify!(args_ty[i], arg_ty, envt, node)
            } else {
              self.check!(arg, args_ty[i], envc, envt, envs)
            }
          }
          self.replace(ret_ty, envt)
        }
        _ =>
          fail!(
            "infer: constructor not a known function type: \{inferred_fun_ty}",
          )
      }
    }
    Syntax::App(Var(f), targs=_, args) as app => {
      let (inferred_fun_ty, tvars) = match envs.find(f) {
        Some(t) => self.inst_and_tvars(t, envt)
        None => fail!("unbound variable: \{f}")
      }
      match inferred_fun_ty {
        Type::Fun(args_ty, ret_ty) => {
          if args.length() != args_ty.length() {
            fail!("infer: different number of arguments")
          }
          for i, arg in args {
            if args_ty[i].is_unknown() {
              let arg_ty = self.infer!(arg, envc, envt, envs)
              self.unify!(args_ty[i], arg_ty, envt, node)
            } else {
              self.check!(arg, args_ty[i], envc, envt, envs)
            }
          }
          app.targs = Some(tvars)
          self.replace(ret_ty, envt)
        }
        Type::Var({ val: None }) => {
          let guessed_ret_ty = Type::new_var()
          let args_ty = []
          for arg in args {
            let arg_ty = self.infer!(arg, envc, envt, envs)
            args_ty.push(arg_ty)
          }
          let guessed_fun_ty = Type::Fun(args_ty, guessed_ret_ty)
          self.unify!(inferred_fun_ty, guessed_fun_ty, envt, node)
          app.targs = Some(tvars)
          self.replace(guessed_ret_ty, envt)
        }
        _ => fail!("infer: not a function type: \{inferred_fun_ty}")
      }
    }
    Syntax::App(f, targs=_, args) => {
      let inferred_fun_ty = self.infer!(f, envc, envt, envs)
      match inferred_fun_ty {
        Type::Fun(args_ty, ret_ty) => {
          if args.length() != args_ty.length() {
            fail!("infer: different number of arguments")
          }
          for i, arg in args {
            if args_ty[i].is_unknown() {
              let arg_ty = self.infer!(arg, envc, envt, envs)
              self.unify!(args_ty[i], arg_ty, envt, node)
            } else {
              self.check!(arg, args_ty[i], envc, envt, envs)
            }
          }
          self.replace(ret_ty, envt)
        }
        Type::Var({ val: None }) => {
          let guessed_ret_ty = Type::new_var()
          let args_ty = []
          for arg in args {
            let arg_ty = self.infer!(arg, envc, envt, envs)
            args_ty.push(arg_ty)
          }
          let guessed_fun_ty = Type::Fun(args_ty, guessed_ret_ty)
          self.unify!(inferred_fun_ty, guessed_fun_ty, envt, node)
          self.replace(guessed_ret_ty, envt)
        }
        _ => fail!("infer: not a function type: \{inferred_fun_ty}")
      }
    }
    Syntax::Get(arr, idx) => {
      let arr_ty = self.infer!(arr, envc, envt, envs)
      match arr_ty {
        Type::Array(ty) => {
          self.check!(idx, Type::Int, envc, envt, envs)
          self.replace(ty, envt)
        }
        _ => fail!("infer: not an array")
      }
    }
    Syntax::If(cond, t, f) => {
      self.check!(cond, Type::Bool, envc, envt, envs)
      let ty1 = self.infer!(t, envc, envt, envs)
      let ty2 = self.infer!(f, envc, envt, envs)
      self.unify!(ty1, ty2, envt, node)
      self.replace(ty1, envt)
    }
    Syntax::Prim(x, y, _, kind=None) as r => {
      let ty1 = self.infer!(x, envc, envt, envs)
      let ty2 = self.infer!(y, envc, envt, envs)
      self.unify!(ty1, ty2, envt, node)
      let ty1 = self.replace(ty1, envt)
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
      self.check!(x, Type::Int, envc, envt, envs)
      self.check!(y, Type::Int, envc, envt, envs)
      Type::Int
    }
    Syntax::Prim(x, y, _, kind=Some(Kind::Double)) => {
      self.check!(x, Type::Double, envc, envt, envs)
      self.check!(y, Type::Double, envc, envt, envs)
      Type::Double
    }
    Syntax::Eq(x, y) => {
      let ty1 = self.infer!(x, envc, envt, envs)
      if ty1.is_unknown() {
        let ty2 = self.infer!(y, envc, envt, envs)
        self.unify!(ty1, ty2, envt, node)
      } else {
        self.check!(y, ty1, envc, envt, envs)
      }
      Type::Bool
    }
    Syntax::LE(x, y) => {
      let ty1 = self.infer!(x, envc, envt, envs)
      if ty1.is_unknown() {
        let ty2 = self.infer!(y, envc, envt, envs)
        self.unify!(ty1, ty2, envt, node)
      } else {
        self.check!(y, ty1, envc, envt, envs)
      }
      Type::Bool
    }
    Syntax::Let((x, t), e1, e2) => {
      let t = if t.is_unknown() {
        let inferred = self.infer!(e1, envc, envt, envs)
        self.unify!(t, inferred, envt, node)
        t
      } else {
        self.check!(e1, t, envc, envt, envs)
        t
      }
      let envs = envs.add(x, t.to_scheme())
      self.infer!(e2, envc, envt, envs)
    }
    Syntax::LetEnum(name, ty, e) => {
      let envt = self.add_enum(name, ty, envt)
      let mut envs = envs
      guard let Type::Enum(_, { val: Some(variants) }) = ty.repr() else {
        _ => fail!("check: \{ty} is not an enum type")
      }
      for variant in variants {
        let (cname, args_ty) = variant
        // Add constructors to type scheme environment.
        // This is for constructors such as 'None' and 'Nil'.
        let variant_ty = Type::Fun(args_ty, ty)
        envs = envs.add(cname, variant_ty.to_scheme())
      }
      self.infer!(e, envc, envt, envs)
    }
    Syntax::LetRec(fundef, e) => {
      let envs = self.let_rec_ty(fundef, envs)
      self.let_rec!(fundef, envc, envt, envs)
      self.infer!(e, envc, envt, envs)
    }
    Syntax::LetTuple(xts, e1, e2) => {
      let t1 = self.infer!(e1, envc, envt, envs)
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
            envs = envs.add(xt.0, self.replace(ts[i], envt).to_scheme())
          }
          self.infer!(e2, envc, envt, envs)
        }
        _ => fail!("infer: not a tuple")
      }
    }
    Syntax::Put(arr, idx, x) => {
      self.check!(
        arr,
        Type::Array(self.infer!(x, envc, envt, envs)),
        envc,
        envt,
        envs,
      )
      self.check!(idx, Type::Int, envc, envt, envs)
      Type::Unit
    }
    Syntax::Match(e, arms) => {
      let et = self.infer!(e, envc, envt, envs)
      let actions_ty = []
      let n = arms.length()
      for arm in arms {
        let pattern = arm.pat
        let action = arm.action
        let arm_envs = self.check_and_bind_pattern!(
          pattern, et, action, envt, envs,
        )
        let action_ty = self.infer!(action, envc, envt, arm_envs)
        actions_ty.push(action_ty)
      }
      // ring unify
      for i in 1..<n {
        self.unify!(actions_ty[i - 1], actions_ty[i], envt, node)
      }
      self.unify!(actions_ty[n - 1], actions_ty[0], envt, node)
      self.replace(actions_ty[0], envt)
    }
    Syntax::LetClass(classdef, e) => {
      let mut envc = envc.add_class(classdef.name, classdef)
      for mtd in classdef.methods {
        let (name, ty) = mtd
        envc = envc.add_method!(name, classdef.name, ty)
      }
      self.infer!(e, envc, envt, envs)
    }
    Syntax::LetInstance({ ty, name, methods }, e) => {
      let cls = match envc.find_class(name) {
        Some(cls) => cls
        None => fail!("infer: unknown class \{name}")
      }
      let inside_envt = envt._.add(cls.tyvar, ty)
      let implemented = @hashset.new()
      for method in methods {
        let (name, body) = method
        let method_ty = match cls.methods.search_by(fn(m) { m.0 == name }) {
          Some(idx) => cls.methods[idx].1
          None => fail!("infer: unknown method \{name}")
        }
        let method_ty = self.replace(method_ty, inside_envt)
        self.check!(body, method_ty, envc, inside_envt, envs)
        implemented.insert(name)
      }
      for method in cls.methods {
        let (name, _) = method
        if implemented.contains(name) == false {
          fail!("infer: method \{name} not implemented")
        }
      }
      self.infer!(e, envc, envt, envs)
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
      App(f, ~targs, args) =>
        App(
          aux(f),
          targs=targs.map(fn(x) { x.map(fn(x) { (x.0, self.subst(x.1)) }) }),
          args.map(aux),
        )
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
      LetClass({ tyvar, name, methods }, e) => {
        let methods = methods.map(
          fn(method) {
            let (name, ty) = method
            (name, self.subst(ty))
          },
        )
        LetClass({ tyvar, name, methods }, aux(e))
      }
      LetInstance({ ty, name, methods }, e) => {
        let ty = self.subst(ty)
        let methods = methods.map(
          fn(method) {
            let (name, body) = method
            (name, aux(body))
          },
        )
        LetInstance({ ty, name, methods }, aux(e))
      }
      Bool(_) | Int(_) | Double(_) | String(_) | Var(_) | Unit => syn
      CtorApp(f, args) => CtorApp(f, args.map(aux))
      Match(e, arms) => {
        let arms = arms.map(
          fn(arm) {
            let { pat, action } = arm
            @types.MatchArm::{ pat, action: aux(action) }
          },
        )
        Match(aux(e), arms)
      }
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
  .add("print_string", @types.Fun([String], Unit))
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
  let syntax = self.resolve!(self.syntax, @immut/hashmap.T::new())
  self.tracer.record("resolved", syntax.to_pretty_print())
  self.check!(syntax, Type::Unit, TyCEnv::new(), @immut/hashmap.new(), envs)
  self.tracer.record("typer", syntax.to_pretty_print())
  let syntax = self.simplify(syntax)
  self.tracer.record("typer_simplified", syntax.to_pretty_print())
  syntax
}
