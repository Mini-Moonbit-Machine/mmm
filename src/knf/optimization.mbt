// new_name, args, fvars, old_type
typealias NameCtx = @immut/hashmap.T[
  Name,
  (Name, Array[Name], Array[Name], Type),
]

typealias TypeCtx = @immut/hashmap.T[Name, Type]

pub struct LambdaLiftEnv {
  name_ctx : NameCtx // old_func_name -> (new_func_name, args, fvars, old_type)
  type_ctx : TypeCtx
  mut free_set_closure : Map[Name, FreeSet]
  top_bindings : LetList
  top_names : FreeSet
  lifted_counter : UInt
  closure_counter : UInt
}

pub fn LambdaLiftEnv::new() -> LambdaLiftEnv {
  LambdaLiftEnv::{
    name_ctx: NameCtx::new(),
    type_ctx: TypeCtx::new(),
    free_set_closure: Map::new(),
    top_bindings: [],
    top_names: FreeSet::new(),
    lifted_counter: 0,
    closure_counter: 0,
  }
}

fn add_name_type(self : LambdaLiftEnv, name : Name, ty : Type) -> LambdaLiftEnv {
  let name_ctx = self.name_ctx
  let type_ctx = self.type_ctx.add(name, ty)
  let free_set_closure = self.free_set_closure
  let top_letrec = self.top_bindings
  let top_names = self.top_names
  let lifted_counter = self.lifted_counter
  let closure_counter = self.closure_counter
  LambdaLiftEnv::{
    name_ctx,
    type_ctx,
    free_set_closure,
    top_bindings: top_letrec,
    top_names,
    lifted_counter,
    closure_counter,
  }
}

fn add_lifted_name(
  self : LambdaLiftEnv,
  old_name : Name,
  args : Array[Name],
  fvars : Array[Name],
  ty0 : Type
) -> (Name, LambdaLiftEnv) {
  let new_name = old_name.to_string() + "l" + self.lifted_counter.to_string()
  let new_name = try {
    Name::from_string!(new_name)
  } catch {
    _ => {
      println("failed to create new name \{new_name}")
      panic()
    }
  }
  let name_ctx = self.name_ctx.add(old_name, (new_name, args, fvars, ty0))
  let type_ctx = self.type_ctx
  let free_set_closure = self.free_set_closure
  let top_letrec = self.top_bindings
  let top_names = self.top_names
  let lifted_counter = self.lifted_counter + 1
  let closure_counter = self.closure_counter
  let env = LambdaLiftEnv::{
    name_ctx,
    type_ctx,
    free_set_closure,
    top_bindings: top_letrec,
    top_names,
    lifted_counter,
    closure_counter,
  }
  (new_name, env)
}

pub fn LambdaLiftEnv::eta_expand(
  self : LambdaLiftEnv,
  name : Name
) -> (LetList, Name, LambdaLiftEnv) {
  match self.name_ctx.find(name) {
    Some((name, args, fvars, ty0)) => {
      let cls_name = name.to_string() + "c" + self.closure_counter.to_string()
      let cls_name = try {
        Name::from_string!(cls_name)
      } catch {
        _ => {
          println("failed to create new name \{cls_name}")
          panic()
        }
      }
      let (ty, args_ty) = match ty0 {
        Fun(args_ty, ty) => (ty, args_ty)
        _ => {
          println("failed to find fun type for \{name}")
          panic()
        }
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

fn LambdaLiftEnv::eta_expand_names(
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

fn LambdaLiftEnv::get_type(self : LambdaLiftEnv, name : Name) -> Type {
  let name = match self.name_ctx.find(name) {
    Some((name, _, _, _)) => name
    None => name
  }
  match self.type_ctx.find(name) {
    Some(ty) => ty
    None => {
      println("failed to find type for \{name} in \{self.type_ctx}")
      panic()
    }
  }
}

fn LambdaLiftEnv::lift(self : LambdaLiftEnv, knf : Knf, top : Bool) -> Knf {
  match knf {
    Unit
    | Int(_)
    | Double(_)
    | Neg(_)
    | FNeg(_)
    | Add(_, _)
    | Sub(_, _)
    | Mul(_, _) | Div(_, _) | FAdd(_, _) | FSub(_, _) | FMul(_, _) | FDiv(_, _) =>
      // we assume that the names here are non-function, otherwise it tends to be a type error
      knf
    IfEq(x, y, k1, k2) => {
      let k1 = self.lift(k1, top)
      let k2 = self.lift(k2, top)
      IfEq(x, y, k1, k2)
    }
    IfLe(x, y, k1, k2) => {
      let k1 = self.lift(k1, top)
      let k2 = self.lift(k2, top)
      IfLe(x, y, k1, k2)
    }
    Let((x, ty), e1, e2) => {
      let e1 = self.lift(e1, false)
      let self = self.add_name_type(x, ty)
      if top {
        self.top_bindings.push(Binding::Let(x, ty, e1))
        self.lift(e2, top)
      } else {
        Let((x, ty), e1, self.lift(e2, top))
      }
    }
    LetTuple(xts, y, k) => {
      let self = xts.fold(
        init=self,
        fn(env, xt) { env.add_name_type(xt.0, xt.1) },
      )
      if top {
        self.top_bindings.push(Binding::LetTuple(xts, y))
        self.lift(k, top)
      } else {
        LetTuple(xts, y, self.lift(k, top))
      }
    }
    ExternalArray(x) => {
      let (list, x, _) = self.eta_expand(x)
      bind_all((ExternalArray(x), Unit, list))
    }
    Tuple(xs) => {
      let (let_list, ys, _) = self.eta_expand_names(xs)
      bind_all((Tuple(ys), Unit, let_list)) // type is unused so just put Unit
    }
    Get(_, _) => knf
    Put(a, b, x) => {
      let (list, x, _) = self.eta_expand(x)
      bind_all((Put(a, b, x), Unit, list))
    }
    Var(x) => {
      let (list, x, _) = self.eta_expand(x)
      bind_all((Var(x), Unit, list))
    }
    ExternalFunctionApplication(f, xs) => {
      let (let_list, ys, _) = self.eta_expand_names(xs)
      bind_all((ExternalFunctionApplication(f, ys), Unit, let_list))
    }
    Apply(f, xs) => {
      let (let_list, ys, self) = self.eta_expand_names(xs)
      match self.name_ctx.find(f) {
        Some((new_name, _, fvars, _)) =>
          bind_all((Apply(new_name, ys + fvars), Unit, let_list))
        None => {
          let (list, f, _) = self.eta_expand(f)
          let_list.append(list)
          bind_all((Apply(f, ys), Unit, let_list))
        }
      }
    }
    LetRec(fdef, e) => {
      let fvars = self.free_set_closure
        .get(fdef.name)
        .or_else(
          fn() {
            println("failed to find free set for \{fdef.name}")
            panic()
          },
        )
      self.top_names.each(fn(name) { fvars.remove(name) })
      fvars.remove(fdef.name)
      let fvars = fvars.iter().collect()
      fvars.sort()
      let new_args_length = fdef.args.length() + fvars.length()
      let old_fun_type = Type::Fun(
        fdef.args.iter().map(fn(arg) { arg.1 }).collect(),
        fdef.ty,
      )
      if new_args_length > 4 {
        // we don't want to lift functions with too many arguments
        let self = self.add_name_type(fdef.name, old_fun_type)
        let body_env = fdef.args.fold(
          init=self,
          fn(env, arg) { env.add_name_type(arg.0, arg.1) },
        )
        let fdef = FuncDef::{
          name: fdef.name,
          ty: fdef.ty,
          args: fdef.args,
          body: body_env.lift(fdef.body, false),
        }
        if top {
          self.top_bindings.push(LetRec(fdef))
          self.lift(e, top)
        } else {
          LetRec(fdef, self.lift(e, top))
        }
      } else {
        let fvars_with_type = fvars.map(
          fn(name) { (name, self.get_type(name)) },
        )
        self.top_names.insert(fdef.name)
        let (new_name, self) = self.add_lifted_name(
          fdef.name,
          fdef.args.map(fn(arg) { arg.0 }),
          fvars,
          Fun(fdef.args.iter().map(fn(arg) { arg.1 }).collect(), fdef.ty),
        )
        let self = self.add_name_type(new_name, old_fun_type)
        let new_args = fdef.args + fvars_with_type
        let body_env = new_args.fold(
          init=self,
          fn(env, arg) { env.add_name_type(arg.0, arg.1) },
        )
        let fdef = FuncDef::{
          name: new_name,
          ty: fdef.ty,
          args: new_args,
          body: body_env.lift(fdef.body, false),
        }
        self.top_bindings.push(LetRec(fdef))
        self.lift(e, top)
      }
    }
  }
}

pub fn go(self : LambdaLiftEnv, knf : Knf) -> Knf {
  let fvc = FreeSetClosure::new()
  let (_, ctx) = fvc.compute(knf)
  self.free_set_closure = ctx
  let knf = self.lift(knf, true)
  bind_all((knf, Unit, self.top_bindings))
}
