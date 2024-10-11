pub typealias LocalEnv = @immut/hashmap.T[Name, LowType]

struct ClosureEnv {
  mut counter : Int
}

pub fn ClosureEnv::new_from_existing(knf : @knf.Knf) -> ClosureEnv {
  { counter: @knf.knf_max_counter(knf) + 1 }
}

fn ClosureEnv::new_temp(self : ClosureEnv) -> Name {
  let temp = Name::slot_only(self.counter)
  self.counter += 1
  temp
}

fn make_label(self : ClosureEnv, name : Name) -> String {
  let _ = self
  match name.name {
    Some(n) => n
    None => "\{name.slot}"
  }
}

enum Binding {
  Let(Name, LowType, Expr)
  LetTuple(Array[(Name, LowType)], Name)
  MakeClosure((Name, LowType), Closure)
}

typealias FuncDefList = Array[FuncDef]

typealias LetList = Array[Binding]

typealias ClosureResult = (Expr, FuncDefList, LetList)

pub fn knf_to_closure(
  self : ClosureEnv,
  knf : @knf.Knf,
  _extern_env : Map[String, Type]
) -> Program!Failure {
  let fundefs = []
  let let_list = []
  let ty_env = LocalEnv::new()
  let r = self.build_closure!(knf, fundefs, let_list, ty_env)
  let body = bind_all(r)
  Program::{ fundefs, body }
}

fn bind_all(cr : ClosureResult) -> Expr {
  let (e, _fundefs, let_list) = cr
  let body = let_list.rev_fold(
    init=e,
    fn(e, bind) {
      match bind {
        Binding::Let(x, ty, e1) => Let((x, ty), e1, e)
        Binding::LetTuple(xts, y) => LetTuple(xts, y, e)
        Binding::MakeClosure((x, ty), c) => MakeClosure((x, ty), c, e)
      }
    },
  )
  body
}

fn type_to_lowtype(ty : Type) -> LowType {
  match ty.repr() {
    Unit => LowType::Unit
    Bool => LowType::Bool
    Int => LowType::Int
    Double => LowType::Double
    Fun(args, ret) =>
      LowType::ClosureFn(args.map(type_to_lowtype), type_to_lowtype(ret)) // Optimizated into direct fn further
    Tuple(tys) => LowType::Tuple(tys.map(type_to_lowtype))
    Array(ty) => LowType::Array(type_to_lowtype(ty))
    Var(ref_ty) =>
      match ref_ty.val {
        Some(t) => type_to_lowtype(t)
        None => abort("Uninstantiated type variable")
      }
    Ptr => LowType::Ptr
  }
}

fn build_closure(
  self : ClosureEnv,
  knf : @knf.Knf,
  func_def_list : FuncDefList,
  let_list : LetList,
  ty_env : LocalEnv
) -> ClosureResult!Failure {
  match knf {
    Unit => (Unit, func_def_list, let_list)
    Int(i) => (Int(i), func_def_list, let_list)
    Double(d) => (Double(d), func_def_list, let_list)
    Neg(x) => (Neg(x), func_def_list, let_list)
    Add(x, y) => (Add(x, y), func_def_list, let_list)
    Sub(x, y) => (Sub(x, y), func_def_list, let_list)
    Mul(x, y) => (Mul(x, y), func_def_list, let_list)
    Div(x, y) => (Div(x, y), func_def_list, let_list)
    FNeg(x) => (FNeg(x), func_def_list, let_list)
    FAdd(x, y) => (FAdd(x, y), func_def_list, let_list)
    FSub(x, y) => (FSub(x, y), func_def_list, let_list)
    FMul(x, y) => (FMul(x, y), func_def_list, let_list)
    FDiv(x, y) => (FDiv(x, y), func_def_list, let_list)
    IfEq(x, y, k1, k2) => {
      let let_k1 = []
      let r1 = self.build_closure!(k1, func_def_list, let_k1, ty_env)
      let let_k2 = []
      let r2 = self.build_closure!(k2, func_def_list, let_k2, ty_env)
      (IfEq(x, y, bind_all(r1), bind_all(r2)), func_def_list, let_list)
    }
    IfLe(x, y, k1, k2) => {
      let let_k1 = []
      let r1 = self.build_closure!(k1, func_def_list, let_k1, ty_env)
      let let_k2 = []
      let r2 = self.build_closure!(k2, func_def_list, let_k2, ty_env)
      (IfLe(x, y, bind_all(r1), bind_all(r2)), func_def_list, let_list)
    }
    Tuple(fields) => (MakeTuple(fields), func_def_list, let_list)
    Get(array, index) => (ArrayGet(array, index), func_def_list, let_list)
    Put(array, index, value) =>
      (ArrayPut(array, index, value), func_def_list, let_list)
    ExternalArray(_) => fail!("ExternalArray not supported")
    ExternalFunctionApplication(name, args) =>
      (CallDirect(name, args), func_def_list, let_list)
    Var(x) => (Var(x), func_def_list, let_list)
    Apply(function, args) =>
      (CallClosure(function, args), func_def_list, let_list)
    LetTuple(xts, y, k) => {
      let new_binder = xts.map(fn(xt) { (xt.0, type_to_lowtype(xt.1)) })
      let_list.push(Binding::LetTuple(new_binder, y))
      // update type environment
      let inner_ty_env = new_binder.fold(
        init=ty_env,
        fn(ty_env, xt) { ty_env.add(xt.0, xt.1) },
      )
      let r = self.build_closure!(k, func_def_list, let_list, inner_ty_env)
      (r.0, r.1, let_list)
    }
    Let((x, ty), e1, e2) => {
      let loty = type_to_lowtype(ty)
      // TODO: lift functional binding to function definition
      let r1 = self.build_closure!(e1, func_def_list, let_list, ty_env)
      let_list.push(Binding::Let(x, loty, r1.0))
      // update type environment
      let inner_ty_env = ty_env.add(x, loty)
      let r2 = self.build_closure!(e2, func_def_list, let_list, inner_ty_env)
      (r2.0, r2.1, let_list)
    }
    LetRec({ name, ty, args, body }, k) as frec => {
      let label = self.make_label(name)
      let loty = type_to_lowtype(ty)
      let args = args.map(fn(arg) { (arg.0, type_to_lowtype(arg.1)) })
      let actual_free_vars = @knf.knf_free_vars(frec).iter().collect()
      let formal_free_vars = actual_free_vars.map(
        fn(name) { (name, ty_env.find(name).unwrap()) },
      )
      let inner_let_list = []
      // update type environment
      let closure_ty = LowType::ClosureFn(args.map(fn(arg) { arg.1 }), loty)
      let ty_env = ty_env.add(name, closure_ty)
      let inner_ty_env = args.fold(
        init=ty_env,
        fn(ty_env, arg) { ty_env.add(arg.0, arg.1) },
      )
      let rb = self.build_closure!(
        body, func_def_list, inner_let_list, inner_ty_env,
      )
      let func_def = FuncDef::{
        name: label,
        old_name: name,
        is_closure: true,
        ty: loty,
        args,
        formal_free_vars,
        body: bind_all(rb),
      }
      func_def_list.push(func_def)
      // bind closure
      let_list.push(
        Binding::MakeClosure(
          (name, closure_ty),
          Closure::{ name: label, actual_free_vars },
        ),
      )
      let r = self.build_closure!(k, func_def_list, let_list, ty_env)
      (r.0, r.1, let_list)
    }
  }
}
