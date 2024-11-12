typealias Syntax = @types.Syntax

typealias Pattern = @types.Pattern

typealias Type = @types.Type

typealias Kind = @types.Kind

typealias TyEnv = @immut/hashmap.T[String, Type]

pub struct Typer {
  syntax : @types.Syntax
  tracer : @util.SubTracer
}

pub fn Typer::new(syntax : Syntax, tracer : @util.SubTracer) -> Typer {
  Typer::{ syntax, tracer }
}

fn Typer::find(ty1 : Type) -> Type {
  ty1.repr()
}

fn Typer::union(ty1 : Type, ty2 : Type) -> Unit!Failure {
  match (ty1, ty2) {
    (Type::Var(t1), Type::Var(_)) => t1.val = Some(ty2)
    (Type::Var(t), _) => t.val = Some(ty2)
    (_, Type::Var(t)) => t.val = Some(ty1)
    _ =>
      fail!("union: unable to union without a type variable: \{ty1} and \{ty2}")
  }
}

fn Typer::unify(ty1 : Type, ty2 : Type, node : Syntax) -> Unit!Failure {
  let ty1 = Typer::find(ty1)
  let ty2 = Typer::find(ty2)
  match (ty1, ty2) {
    (Type::Unit, Type::Unit) => ()
    (Type::Bool, Type::Bool) => ()
    (Type::Int, Type::Int) => ()
    (Type::Double, Type::Double) => ()
    (Type::Fun(args1, ret1), Type::Fun(args2, ret2)) => {
      if args1.length() != args2.length() {
        fail!("unify: different number of arguments")
      }
      for i, x in args1 {
        Typer::unify!(x, args2[i], node)
      }
      Typer::unify!(ret1, ret2, node)
    }
    (Type::Tuple(xs), Type::Tuple(ys)) => {
      if xs.length() != ys.length() {
        fail!("unify: different number of elements")
      }
      for i, x in xs {
        Typer::unify!(x, ys[i], node)
      }
    }
    (Type::Array(x), Type::Array(y)) => Typer::unify!(x, y, node)
    (Type::Var(_), _) => Typer::union!(ty1, ty2)
    (_, Type::Var(_)) => Typer::union!(ty1, ty2)
    _ =>
      fail!("unify: mismatched types \{ty1} and \{ty2}, when handling \{node}")
  }
}

fn Typer::check(node : Syntax, ty : Type, env : TyEnv) -> Unit!Failure {
  let ty = Typer::find(ty)
  if ty.is_unknown() {
    fail!("check \{node} against unknown type")
  }
  match (node, ty) {
    (Syntax::Unit, Type::Unit) => ()
    (Syntax::Bool(_), Type::Bool) => ()
    (Syntax::Int(_), Type::Int) => ()
    (Syntax::Double(_), Type::Double) => ()
    (Syntax::Var(x), ty) =>
      match env.find(x) {
        Some(t) => Typer::unify!(t, ty, node)
        None => fail!("unbound variable: \{x}")
      }
    (Syntax::Tuple(xs), Type::Tuple(ys)) => {
      if xs.length() != ys.length() {
        fail!("check: different number of elements")
      }
      for i, x in xs {
        Typer::check!(x, ys[i], env)
      }
    }
    (Syntax::Not(x), Type::Bool) => Typer::check!(x, Type::Bool, env)
    (Syntax::Array(len, elem), Type::Array(y)) => {
      Typer::check!(elem, y, env)
      Typer::check!(len, Type::Int, env)
    }
    (Syntax::Neg(x, kind=None | Some(Kind::Int)) as r, Type::Int) => {
      Typer::check!(x, Type::Int, env)
      r.kind = Some(Kind::Int)
    }
    (Syntax::Neg(x, kind=None | Some(Kind::Double)) as r, Type::Double) => {
      Typer::check!(x, Type::Double, env)
      r.kind = Some(Kind::Double)
    }
    (Syntax::App(f, args), ty) => {
      let args_ty = []
      for arg in args {
        let arg_ty = Typer::infer!(arg, env)
        args_ty.push(arg_ty)
      }
      let expected_fun_ty = Type::Fun(args_ty, ty)
      Typer::check!(f, expected_fun_ty, env)
    }
    (Syntax::Get(arr, idx), ty) => {
      Typer::check!(arr, Type::Array(ty), env)
      Typer::check!(idx, Type::Int, env)
    }
    (Syntax::If(cond, t, f), ty) => {
      Typer::check!(cond, Type::Bool, env)
      Typer::check!(t, ty, env)
      Typer::check!(f, ty, env)
    }
    (Syntax::Prim(x, y, _, kind=None | Some(Kind::Int)) as r, Type::Int) => {
      Typer::check!(x, Type::Int, env)
      Typer::check!(y, Type::Int, env)
      r.kind = Some(Kind::Int)
    }
    (Syntax::Prim(x, y, _, kind=None | Some(Kind::Double)) as r, Type::Double) => {
      Typer::check!(x, Type::Double, env)
      Typer::check!(y, Type::Double, env)
      r.kind = Some(Kind::Double)
    }
    (Syntax::Eq(x, y), Type::Bool) => {
      let t1 = Typer::infer!(x, env)
      let t2 = Typer::infer!(y, env)
      Typer::unify!(t1, t2, node)
    }
    (Syntax::LE(x, y), Type::Bool) => {
      let t1 = Typer::infer!(x, env)
      let t2 = Typer::infer!(y, env)
      Typer::unify!(t1, t2, node)
    }
    (Syntax::Let((x, t), e1, e2), ty) => {
      let t = if t.is_unknown() {
        let inferred = Typer::infer!(e1, env)
        Typer::unify!(t, inferred, node)
        t
      } else {
        Typer::check!(e1, t, env)
        t
      }
      let env = env.add(x, t)
      Typer::check!(e2, ty, env)
    }
    (Syntax::LetRec(fundef, e), ty) => {
      let guessed_ty = Type::Fun(
        fundef.args.iter().map(fn(x) { x.1 }).collect(),
        fundef.name.1,
      )
      let env = env.add(fundef.name.0, guessed_ty)
      let old_env = env
      let mut env = env
      for xt in fundef.args {
        env = env.add(xt.0, xt.1)
      }
      if fundef.name.1.is_unknown() {
        let t = Typer::infer!(fundef.body, env)
        Typer::unify!(t, fundef.name.1, node)
      } else {
        Typer::check!(fundef.body, fundef.name.1, env)
      }
      Typer::check!(e, ty, old_env)
    }
    (Syntax::LetTuple(xts, e1, e2), t2) =>
      if xts.iter().all(fn(xt) { xt.1.is_unknown() == false }) {
        let t1 = Type::Tuple(xts.iter().map(fn(xt) { xt.1 }).collect())
        Typer::check!(e1, t1, env)
        let mut env = env
        for xt in xts {
          env = env.add(xt.0, Typer::find(xt.1))
        }
        Typer::check!(e2, t2, env)
      } else {
        let t1 = Typer::infer!(e1, env)
        match t1 {
          Type::Tuple(ts) => {
            if xts.length() != ts.length() {
              fail!("check: different number of elements")
            }
            for i, xt in xts {
              match xt {
                (_, t) => Typer::unify!(t, ts[i], node)
              }
            }
            let mut env = env
            for i, xt in xts {
              env = env.add(xt.0, Typer::find(ts[i]))
            }
            Typer::check!(e2, t2, env)
          }
          _ => fail!("check: not a tuple")
        }
      }
    (Syntax::Put(arr, idx, x), Type::Unit) => {
      Typer::check!(arr, Type::Array(ty), env)
      Typer::check!(idx, Type::Int, env)
      Typer::check!(x, ty, env)
    }
    (Syntax::Match(e, arms), ty) => {
      let et = Typer::infer!(e, env)
      for arm in arms {
        let pattern = arm.pat
        let action = arm.action
        let arm_env = Typer::check_and_bind_pattern!(pattern, et, action, env)
        Typer::check!(action, ty, arm_env)
      }
      
    }
    _ => fail!("check: mismatched types \{node} and \{ty}")
  }
}

fn Typer::check_and_bind_pattern(
  pattern: Pattern,
  pt: Type,
  action: Syntax,
  env: TyEnv
) -> TyEnv!Failure {
  match pattern {
    Pattern::Wildcard => env
    Pattern::Unit => {
      Typer::unify!(pt, Type::Unit, action)
      env
    }
    Pattern::Bool(_) => {
      Typer::unify!(pt, Type::Bool, action)
      env
    }
    Pattern::Int(_) => {
      Typer::unify!(pt, Type::Int, action)
      env
    }
    Pattern::Double(_) => {
      Typer::unify!(pt, Type::Double, action)
      env
    }
    Pattern::Var(x) => {
      let env = env.add(x, pt)
      env
    }
    Pattern::Tuple(ps) => {
      match pt {
        Type::Tuple(ts) => {
          if ps.length() != ts.length() {
            fail!("check_pattern: different number of elements")
          }
          let mut new_env = env
          for i, p in ps {
            new_env = Typer::check_and_bind_pattern!(p, ts[i], action, new_env)
          }
          env
        }
        _ => fail!("check pattern: \{pattern} not a tuple")
      }
    }
    _ => fail!("ctor app are not supported yet")
  }
}

fn Typer::infer(node : Syntax, env : TyEnv) -> Type!Failure {
  match node {
    Syntax::Unit => Type::Unit
    Syntax::Bool(_) => Type::Bool
    Syntax::Int(_) => Type::Int
    Syntax::Double(_) => Type::Double
    Syntax::Var(x) =>
      match env.find(x) {
        Some(t) => Typer::find(t)
        None => fail!("unbound variable: \{x}")
      }
    Syntax::Tuple(xs) => {
      let ts = []
      for x in xs {
        ts.push(Typer::infer!(x, env))
      }
      Type::Tuple(ts)
    }
    Syntax::Not(x) => {
      Typer::check!(x, Type::Bool, env)
      Type::Bool
    }
    Syntax::Array(len, elem) => {
      let elem_ty = Typer::infer!(elem, env)
      Typer::check!(len, Type::Int, env)
      Type::Array(elem_ty)
    }
    Syntax::Neg(x, kind=None) as r => {
      let ty = Typer::infer!(x, env)
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
      ty
    }
    Syntax::Neg(x, kind=Some(Kind::Int)) => {
      Typer::check!(x, Type::Int, env)
      Type::Int
    }
    Syntax::Neg(x, kind=Some(Kind::Double)) => {
      Typer::check!(x, Type::Double, env)
      Type::Double
    }
    Syntax::App(f, args) => {
      let inferred_fun_ty = Typer::infer!(f, env)
      match inferred_fun_ty {
        Type::Fun(args_ty, ret_ty) => {
          if args.length() != args_ty.length() {
            fail!("infer: different number of arguments")
          }
          for i, arg in args {
            Typer::check!(arg, args_ty[i], env)
          }
          Typer::find(ret_ty)
        }
        Type::Var({ val: None }) => {
          let guessed_ret_ty = Type::Var(Ref::new(None))
          let args_ty = []
          for arg in args {
            let arg_ty = Typer::infer!(arg, env)
            args_ty.push(arg_ty)
          }
          let guessed_fun_ty = Type::Fun(args_ty, guessed_ret_ty)
          Typer::unify!(inferred_fun_ty, guessed_fun_ty, node)
          Typer::find(guessed_ret_ty)
        }
        _ => fail!("infer: not a function type: \{inferred_fun_ty}")
      }
    }
    Syntax::Get(arr, idx) => {
      let arr_ty = Typer::infer!(arr, env)
      match arr_ty {
        Type::Array(ty) => {
          Typer::check!(idx, Type::Int, env)
          Typer::find(ty)
        }
        _ => fail!("infer: not an array")
      }
    }
    Syntax::If(cond, t, f) => {
      Typer::check!(cond, Type::Bool, env)
      let ty1 = Typer::infer!(t, env)
      let ty2 = Typer::infer!(f, env)
      Typer::unify!(ty1, ty2, node)
      Typer::find(ty1)
    }
    Syntax::Prim(x, y, _, kind=None) as r => {
      let ty1 = Typer::infer!(x, env)
      let ty2 = Typer::infer!(y, env)
      Typer::unify!(ty1, ty2, node)
      let ty1 = Typer::find(ty1)
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
      Typer::check!(x, Type::Int, env)
      Typer::check!(y, Type::Int, env)
      Type::Int
    }
    Syntax::Prim(x, y, _, kind=Some(Kind::Double)) => {
      Typer::check!(x, Type::Double, env)
      Typer::check!(y, Type::Double, env)
      Type::Double
    }
    Syntax::Eq(x, y) => {
      let ty1 = Typer::infer!(x, env)
      if ty1.is_unknown() {
        let ty2 = Typer::infer!(y, env)
        Typer::unify!(ty1, ty2, node)
      } else {
        Typer::check!(y, ty1, env)
      }
      Type::Bool
    }
    Syntax::LE(x, y) => {
      let ty1 = Typer::infer!(x, env)
      if ty1.is_unknown() {
        let ty2 = Typer::infer!(y, env)
        Typer::unify!(ty1, ty2, node)
      } else {
        Typer::check!(y, ty1, env)
      }
      Type::Bool
    }
    Syntax::Let((x, t), e1, e2) => {
      let t = if t.is_unknown() {
        let inferred = Typer::infer!(e1, env)
        Typer::unify!(t, inferred, node)
        t
      } else {
        Typer::check!(e1, t, env)
        t
      }
      let env = env.add(x, t)
      Typer::infer!(e2, env)
    }
    Syntax::LetRec(fundef, e) => {
      let guessed_ty = Type::Fun(
        fundef.args.iter().map(fn(x) { x.1 }).collect(),
        fundef.name.1,
      )
      let env = env.add(fundef.name.0, guessed_ty)
      let old_env = env
      let mut env = env
      for xt in fundef.args {
        env = env.add(xt.0, xt.1)
      }
      if fundef.name.1.is_unknown() {
        let t = Typer::infer!(fundef.body, env)
        Typer::unify!(fundef.name.1, t, node)
      } else {
        Typer::check!(fundef.body, fundef.name.1, env)
      }
      Typer::infer!(e, old_env)
    }
    Syntax::LetTuple(xts, e1, e2) => {
      let t1 = Typer::infer!(e1, env)
      match t1 {
        Type::Tuple(ts) => {
          if xts.length() != ts.length() {
            fail!("infer: different number of elements")
          }
          for i, xt in xts {
            match xt {
              (_, t) => Typer::unify!(t, ts[i], node)
            }
          }
          let mut env = env
          for i, xt in xts {
            env = env.add(xt.0, Typer::find(ts[i]))
          }
          Typer::infer!(e2, env)
        }
        _ => fail!("infer: not a tuple")
      }
    }
    Syntax::Put(arr, idx, x) => {
      Typer::check!(arr, Type::Array(Typer::infer!(x, env)), env)
      Typer::check!(idx, Type::Int, env)
      Type::Unit
    }
  }
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

pub fn Typer::check_all(self : Typer) -> Unit!Failure {
  Typer::check!(self.syntax, Type::Unit, externals())
}
