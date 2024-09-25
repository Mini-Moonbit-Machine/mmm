pub typealias LocalEnv = @immut/hashmap.T[Name, LowType]

struct ClosureEnv {
  mut counter : Int
}

pub fn ClosureEnv::new_from_existing(
  knf: @knf.Knf,
) -> ClosureEnv {
  { counter: @knf.knf_max_counter(knf) + 1}
}

fn ClosureEnv::new_temp(self : ClosureEnv) -> Name {
  let temp = Name::slot_only(self.counter)
  self.counter += 1
  temp
}

fn make_label(self: ClosureEnv, name: Name) -> String {
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

typealias FreeSet = @hashset.T[Name]
fn knf_free_vars(k : @knf.Knf) -> FreeSet {
  match k {
    Unit | Int(_) | Double(_) => FreeSet::new()
    Neg(x) | FNeg(x) => FreeSet::of([x])
    Add(x, y) | Sub(x, y) | Mul(x, y) | Div(x, y) | FAdd(x, y) | FSub(x, y) | FMul(x, y) | FDiv(x, y) => {
      FreeSet::of([x, y])
    }
    IfEq(x, y, k1, k2) | IfLe(x, y, k1, k2) => {
      let fv1 = knf_free_vars(k1)
      let fv2 = knf_free_vars(k2)
      let fv = fv1.union(fv2)
      fv.insert(x)
      fv.insert(y)
      fv
    }
    Var(x) => FreeSet::of([x])
    Apply(f, args) => {
      let fv = FreeSet::from_array(args)
      fv.insert(f)
      fv
    }
    Tuple(fields) => {
      FreeSet::from_array(fields)
    }
    Let((x, _), e1, e2) => {
      let fv2 = knf_free_vars(e2)
      let fv1 = knf_free_vars(e1)
      let fv = fv1.union(fv2)
      // x is bounded
      fv.remove(x)
      fv
    }
    LetRec(f, k) => {
      let fv2 = knf_free_vars(k)
      let fv1 = knf_free_vars(f.body)
      let fv = fv1.union(fv2)
      // f, args is bounded
      f.args.each(fn (argt) { fv.remove(argt.0)})
      fv.remove(f.name)
      fv
    }
    LetTuple(xts, y, k) => {
      let fv = knf_free_vars(k)
      fv.insert(y)
      // xts is bounded
      xts.each(fn (xt) { fv.remove(xt.0) })
      fv
    }
    Get(array, index) => FreeSet::of([array, index])
    Put(array, index, value) => FreeSet::of([array, index, value])
    ExternalArray(_) => FreeSet::new()
    ExternalFunctionApplication(_, args) => FreeSet::from_array(args)
  }
}

pub fn knf_to_closure(
  self: ClosureEnv,
  knf: @knf.Knf,
  extern_env : Map[String, Type]
) -> Program!Failure {
  let fundefs = Array::new()
  let let_list = Array::new()
  let ty_env = LocalEnv::new()
  let r = self.build_closure!(knf, fundefs, let_list, ty_env)
  let body = bind_all(r)
  Program::{ fundefs, body }
}

fn bind_all(cr: ClosureResult) -> Expr {
  let (e, fundefs, let_list) = cr
  let body = let_list.rev_fold(init=e, fn (e, bind) {
    match bind {
      Binding::Let(x, ty, e1) => Let((x, ty), e1, e)
      Binding::LetTuple(xts, y) => LetTuple(xts, y, e)
      Binding::MakeClosure((x, ty), c) => MakeClosure((x, ty), c, e)
    }
  })
  body
}

fn type_to_lowtype(ty : Type) -> LowType {
  match ty {
    Unit => LowType::Unit
    Bool => LowType::Bool
    Int => LowType::Int
    Double => LowType::Double
    Fun(args, ret) => LowType::ClosureFn(args.map(type_to_lowtype), type_to_lowtype(ret)) // Optimizated into direct fn further
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
  self: ClosureEnv,
  knf : @knf.Knf,
  func_def_list: FuncDefList,
  let_list: LetList,
  ty_env : LocalEnv
) -> ClosureResult!Failure {
  match knf {
    Unit => (Unit, func_def_list, let_list)
    Int(i) => (Int(i), func_def_list, let_list)
    Double(d)  => (Double(d), func_def_list, let_list)
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
      let (e1, func_def_list, let_list) = self.build_closure!(k1, func_def_list, let_list, ty_env)
      let (e2, func_def_list, let_list) = self.build_closure!(k2, func_def_list, let_list, ty_env)
      (IfEq(x, y, e1, e2), func_def_list, let_list)
    }
    IfLe(x, y, k1, k2) => {
      let (e1, func_def_list, let_list) = self.build_closure!(k1, func_def_list, let_list, ty_env)
      let (e2, func_def_list, let_list) = self.build_closure!(k2, func_def_list, let_list, ty_env)
      (IfLe(x, y, e1, e2), func_def_list, let_list)
    }
    Tuple(fields) => (MakeTuple(fields), func_def_list, let_list)
    Get(array, index) => (ArrayGet(array, index), func_def_list, let_list)
    Put(array, index, value) => (ArrayPut(array, index, value), func_def_list, let_list)
    ExternalArray(_) => fail!("ExternalArray not supported")
    ExternalFunctionApplication(name, args) => (CallDirect(name, args), func_def_list, let_list)
    Var(x) => (Var(x), func_def_list, let_list)
    Apply(function, args) => (CallClosure(function, args), func_def_list, let_list)
    LetTuple(xts, y, k) => {
      let new_binder = xts.map(
        fn (xt) {
          (xt.0, type_to_lowtype(xt.1))
        }
      )
      let_list.push(Binding::LetTuple(new_binder, y))
      // update type environment
      let inner_ty_env = new_binder.fold(init=ty_env, fn (ty_env, xt) {
        ty_env.add(xt.0, xt.1)
      })
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
    LetRec({name, ty, args, body}, k) as frec => {
      let label = self.make_label(name)
      let loty = type_to_lowtype(ty)
      let args = args.map(fn (arg) { (arg.0, type_to_lowtype(arg.1)) })
      let actual_free_vars = knf_free_vars(frec).iter().collect()
      let formal_free_vars = actual_free_vars
        .map(fn (name) { (name, ty_env.find(name).unwrap()) })

      let inner_let_list = []
      // update type environment
      let closure_ty =  LowType::ClosureFn(
        args.map(fn (arg) { arg.1 }),
        loty
      )
      let ty_env = ty_env.add(
        name,
        closure_ty
      )
      let inner_ty_env = args.fold(init=ty_env, fn (ty_env, arg) {
        ty_env.add(arg.0, arg.1)
      })
      let rb = self.build_closure!(body, func_def_list, inner_let_list, inner_ty_env)
      let func_def = FuncDef:: {
        name: label,
        old_name: name,
        is_closure: true,
        ty: loty,
        args: args,
        formal_free_vars: formal_free_vars,
        body: bind_all(rb)
      }
      func_def_list.push(func_def)
      // bind closure
      let_list.push(
        Binding::MakeClosure(
          (name, closure_ty),
          Closure:: {
            name: label,
            actual_free_vars: actual_free_vars
          }
        )
      )
      let r = self.build_closure!(k, func_def_list, let_list, ty_env)
      (r.0, r.1, let_list)
    }
  }
}