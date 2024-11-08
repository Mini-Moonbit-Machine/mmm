// new_name, args, fvars, old_type
typealias NameCtx = @immut/hashmap.T[
  Name,
  (Name, Array[Name], Array[Name], Type),
]

typealias TypeCtx = @immut/hashmap.T[Name, Type]

pub struct LambdaLifter {
  knf : Knf
  mut free_set_closure : Map[Name, FreeSet]
  knf_env : KnfEnv
  top_bindings : LetList
  top_names : FreeSet
  tracer : @util.SubTracer
}

pub struct LambdaLiftEnv {
  name_ctx : NameCtx // old_func_name -> (new_func_name, args, fvars, old_type)
  type_ctx : TypeCtx
  knf_env : KnfEnv
  tracer : @util.SubTracer
}

pub fn LambdaLifter::new(tracer : @util.SubTracer, knf : Knf) -> LambdaLifter {
  let knf_env = KnfEnv::new(@typer.externals(), tracer)
  knf_env.init_counter_from_existing(knf)
  let top_bindings = []
  let top_names = FreeSet::new()
  LambdaLifter::{
    knf,
    free_set_closure: Map::new(),
    knf_env,
    tracer,
    top_bindings,
    top_names,
  }
}

pub fn LambdaLiftEnv::new(
  knf_env : KnfEnv,
  tracer : @util.SubTracer
) -> LambdaLiftEnv {
  { name_ctx: NameCtx::new(), type_ctx: TypeCtx::new(), knf_env, tracer }
}

fn add_name_type(self : LambdaLiftEnv, name : Name, ty : Type) -> LambdaLiftEnv {
  let type_ctx = self.type_ctx.add(name, ty)
  { ..self, type_ctx, }
}

fn add_lifted_name(
  self : LambdaLiftEnv,
  old_name : Name,
  args : Array[Name],
  fvars : Array[Name],
  ty0 : Type
) -> (Name, LambdaLiftEnv) {
  let new_name = self.knf_env.new_named_temp(old_name)
  let name_ctx = self.name_ctx.add(old_name, (new_name, args, fvars, ty0))
  let env = { ..self, name_ctx, }
  (new_name, env)
}

pub fn eta_expand(
  self : LambdaLiftEnv,
  name : Name
) -> (LetList, Name, LambdaLiftEnv) {
  match self.name_ctx.find(name) {
    Some((name, args, fvars, ty0)) => {
      let cls_name = self.knf_env.new_named_temp(name)
      let (ty, args_ty) = match ty0 {
        Fun(args_ty, ty) => (ty, args_ty)
        _ => self.tracer.abort("failed to find fun type for \{name}")
      }
      let zipped_args = []
      for i, arg in args {
        let arg_ty = args_ty[i]
        zipped_args.push((arg, arg_ty))
      }
      let fdef = FuncDef::{
        name: cls_name,
        ty,
        args: zipped_args,
        body: Apply(name, args + fvars),
      }
      ([Binding::LetRec(fdef)], cls_name, self)
    }
    None => ([], name, self)
  }
}

fn eta_expand_names(
  self : LambdaLiftEnv,
  xs : Array[Name]
) -> (LetList, Array[Name], LambdaLiftEnv) {
  let mut self = self
  let ys = []
  let let_list = []
  for x in xs {
    let (list, y, self2) = self.eta_expand(x)
    self = self2
    ys.push(y)
    let_list.append(list)
  }
  (let_list, ys, self)
}

fn get_type(self : LambdaLiftEnv, name : Name) -> Type {
  let name = match self.name_ctx.find(name) {
    Some((name, _, _, _)) => name
    None => name
  }
  match self.type_ctx.find(name) {
    Some(ty) => ty
    None =>
      self.tracer.abort("failed to find type for \{name} in \{self.type_ctx}")
  }
}

fn LambdaLifter::lift(
  self : LambdaLifter,
  env : LambdaLiftEnv,
  knf : Knf,
  top : Bool
) -> (LetList, Knf) {
  match knf {
    // we assume that the names here are non-function, otherwise it tends to be a type error
    Unit | Bool(_) | Int(_) | Double(_) => ([], knf)
    Neg(_) | FNeg(_) => ([], knf)
    Add(_, _)
    | Sub(_, _)
    | Mul(_, _) | Div(_, _) | FAdd(_, _) | FSub(_, _) | FMul(_, _) | FDiv(_, _) =>
      ([], knf)
    IfEq(x, y, k1, k2) => {
      let (l1, k1) = self.lift(env, k1, top)
      let (l2, k2) = self.lift(env, k2, top)
      let k1 = bind_all((k1, Unit, l1))
      let k2 = bind_all((k2, Unit, l2))
      ([], IfEq(x, y, k1, k2))
    }
    IfLe(x, y, k1, k2) => {
      let (l1, k1) = self.lift(env, k1, top)
      let (l2, k2) = self.lift(env, k2, top)
      let k1 = bind_all((k1, Unit, l1))
      let k2 = bind_all((k2, Unit, l2))
      ([], IfLe(x, y, k1, k2))
    }
    Let((x, ty), e1, e2) => {
      let (l, e1) = self.lift(env, e1, false)
      l.push(Binding::Let(x, ty, e1))
      let env = env.add_name_type(x, ty)
      if top {
        self.top_bindings.append(l)
        self.top_names.insert(x)
        self.lift(env, e2, top)
      } else {
        let (l2, e2) = self.lift(env, e2, top)
        l.append(l2)
        (l, e2)
      }
    }
    LetTuple(xts, y, k) => {
      let env = xts.fold(
        init=env,
        fn(env, xt) { env.add_name_type(xt.0, xt.1) },
      )
      if top {
        self.top_bindings.push(Binding::LetTuple(xts, y))
        self.lift(env, k, top)
      } else {
        let l = [Binding::LetTuple(xts, y)]
        let (l2, k) = self.lift(env, k, top)
        l.append(l2)
        (l, k)
      }
    }
    ExternalArray(x) => {
      let (l, x, _) = env.eta_expand(x)
      (l, ExternalArray(x))
    }
    Tuple(xs) => {
      let (l, ys, _) = env.eta_expand_names(xs)
      (l, Tuple(ys))
    }
    Get(_, _) => ([], knf)
    Put(a, b, x) => {
      let (l, x, _) = env.eta_expand(x)
      (l, Put(a, b, x))
    }
    Var(x) => {
      let (l, x, _) = env.eta_expand(x)
      (l, Var(x))
    }
    ExternalFunctionApplication(f, xs) => {
      let (l, ys, _) = env.eta_expand_names(xs)
      (l, ExternalFunctionApplication(f, ys))
    }
    Apply(f, xs) => {
      let (l, ys, env) = env.eta_expand_names(xs)
      match env.name_ctx.find(f) {
        Some((new_name, _, fvars, _)) => (l, Apply(new_name, ys + fvars))
        None => {
          let (l1, f, _) = env.eta_expand(f)
          l.append(l1)
          (l, Apply(f, ys))
        }
      }
    }
    LetRec(fdef, e) => {
      let fvars = self.free_set_closure
        .get(fdef.name)
        .or_else(
          fn() { self.tracer.abort("failed to find free set for \{fdef.name}") },
        )
      self.top_names.each(fn(name) { fvars.remove(name) })
      fvars.remove(fdef.name)
      let fvars = fvars.iter().collect()
      fvars.sort()
      let old_fun_type = Type::Fun(
        fdef.args.iter().map(fn(arg) { arg.1 }).collect(),
        fdef.ty,
      )
      let fvars_with_type = fvars.map(fn(name) { (name, env.get_type(name)) })
      let new_args = fdef.args + fvars_with_type
      let new_float_args_length = new_args
        .filter(fn(arg) { arg.1.is_float_like() })
        .length()
      let new_integer_args_length = new_args.length() - new_float_args_length
      // we don't want to lift functions with too many arguments
      if new_integer_args_length >= 10 || new_float_args_length >= 10 {
        let env = env.add_name_type(fdef.name, old_fun_type)
        let body_env = fdef.args.fold(
          init=env,
          fn(env, arg) { env.add_name_type(arg.0, arg.1) },
        )
        let (l, body_e) = self.lift(body_env, fdef.body, false)
        let fdef = FuncDef::{
          name: fdef.name,
          ty: fdef.ty,
          args: fdef.args,
          body: bind_all((body_e, Unit, l)),
        }
        if top {
          self.top_names.insert(fdef.name)
          self.top_bindings.push(LetRec(fdef))
          self.lift(env, e, top)
        } else {
          let l = [Binding::LetRec(fdef)]
          let (l2, e) = self.lift(env, e, top)
          l.append(l2)
          (l, e)
        }
      } else {
        self.top_names.insert(fdef.name)
        let (new_name, env) = env.add_lifted_name(
          fdef.name,
          fdef.args.map(fn(arg) { arg.0 }),
          fvars,
          Fun(fdef.args.iter().map(fn(arg) { arg.1 }).collect(), fdef.ty),
        )
        let env = env.add_name_type(new_name, old_fun_type)
        let body_env = new_args.fold(
          init=env,
          fn(env, arg) { env.add_name_type(arg.0, arg.1) },
        )
        let (l, body_e) = self.lift(body_env, fdef.body, false)
        let fdef = FuncDef::{
          name: new_name,
          ty: fdef.ty,
          args: new_args,
          body: bind_all((body_e, Unit, l)),
        }
        self.top_bindings.push(LetRec(fdef))
        self.lift(env, e, top)
      }
    }
  }
}

fn count_let_rec(knf : Knf) -> Int {
  match knf {
    Unit
    | Bool(_)
    | Int(_)
    | Double(_)
    | Neg(_)
    | Add(_, _)
    | Sub(_, _)
    | Mul(_, _)
    | Div(_, _) | FNeg(_) | FAdd(_, _) | FSub(_, _) | FMul(_, _) | FDiv(_, _) =>
      0
    IfEq(_, _, c, d) => count_let_rec(c) + count_let_rec(d)
    IfLe(_, _, c, d) => count_let_rec(c) + count_let_rec(d)
    Let((_, _), c, d) => count_let_rec(c) + count_let_rec(d)
    Var(_) => 0
    LetRec(f, e) => 1 + count_let_rec(e) + count_let_rec(f.body)
    Apply(_, _) => 0
    Tuple(_) => 0
    LetTuple(_, _, e) => count_let_rec(e)
    Get(_, _) => 0
    Put(_, _, _) => 0
    ExternalArray(_) => 0
    ExternalFunctionApplication(_, _) => 0
  }
}

pub fn go(self : LambdaLifter) -> Knf {
  let knf = self.knf
  let n1 = count_let_rec(knf)
  let old_knf = knf
  let fvc = FreeSetClosure::new()
  let (_, ctx) = fvc.compute(knf)
  self.tracer.record("free_set_closure", ctx)
  self.free_set_closure = ctx
  let env = LambdaLiftEnv::new(self.knf_env, self.tracer)
  let (l, knf) = self.lift(env, knf, true)
  if l.length() > 0 {
    self.tracer.abort("top level let list should be empty")
  }
  let knf = bind_all((knf, Unit, self.top_bindings))
  let n2 = count_let_rec(knf)
  let diff = n2 - n1
  self.tracer.println("Numbers of let rec: \{n1} \{n2} \{diff}")
  self.tracer.record("knf_after_lifting", knf.to_pretty_print())
  let knf = if diff > 0 && diff.to_double() / n1.to_double() >= 0.4 {
    self.tracer.println(
      "Give up lambda lifting because of too many let rec produced",
    )
    old_knf
  } else {
    knf
  }
  knf
}
