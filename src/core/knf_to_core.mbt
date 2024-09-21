typealias Knf = @knf.Knf

typealias NameCtx = @immut/hashmap.T[@knf.Name, Value]

typealias TypeCtx = @immut/hashmap.T[@knf.Name, Type]

pub struct CoreEnv {
  mut func_counter : Ref[Int]
  mut func_fvar_counter : Int
  mut func_insn_counter : Int
  mut func_block_counter : Int
  mut func_mem_counter : Int
  mut lifted_func_counter : Int
} derive(Show)

pub struct Ctx {
  // A name should always be in ty_ctx
  // It should be in name_ctx if it is a local name
  // Otherwise, it should be a free variable or a top-level name
  // After seeing a free variable, we regard it as a local name
  // Thus, we can re-use its value later
  ty_ctx : TypeCtx // types of names
  name_ctx : NameCtx // local names
  top_name_ctx : NameCtx // top-level names
} derive(Show)

fn Ctx::new() -> Ctx {
  {
    ty_ctx: TypeCtx::new(),
    name_ctx: NameCtx::new(),
    top_name_ctx: NameCtx::new(),
  }
}

fn Ctx::add_local_name_value_ty(
  self : Ctx,
  name : @knf.Name,
  val : Value,
  ty : Type
) -> Ctx {
  {
    ty_ctx: self.ty_ctx.add(name, ty),
    name_ctx: self.name_ctx.add(name, val),
    top_name_ctx: self.top_name_ctx,
  }
}

fn Ctx::add_name_ty(self : Ctx, name : @knf.Name, ty : Type) -> Ctx {
  {
    ty_ctx: self.ty_ctx.add(name, ty),
    name_ctx: self.name_ctx,
    top_name_ctx: self.top_name_ctx,
  }
}

fn Ctx::add_local_name_value(self : Ctx, name : @knf.Name, val : Value) -> Ctx {
  {
    ty_ctx: self.ty_ctx,
    name_ctx: self.name_ctx.add(name, val),
    top_name_ctx: self.top_name_ctx,
  }
}

fn Ctx::add_top_name_value(self : Ctx, name : @knf.Name, val : Value) -> Ctx {
  {
    ty_ctx: self.ty_ctx,
    name_ctx: self.name_ctx,
    top_name_ctx: self.top_name_ctx.add(name, val),
  }
}

// When entering a function definition, we need to create a new local name context
fn Ctx::enter_func_def(self : Ctx) -> Ctx {
  {
    ty_ctx: self.ty_ctx,
    name_ctx: NameCtx::new(),
    top_name_ctx: self.top_name_ctx,
  }
}

pub fn CoreEnv::new() -> CoreEnv {
  CoreEnv::{
    func_counter: Ref::new(0),
    func_fvar_counter: 0,
    func_insn_counter: 0,
    func_block_counter: 0,
    func_mem_counter: 0,
    lifted_func_counter: 0,
  }
}

pub fn inherit(self : CoreEnv) -> CoreEnv {
  CoreEnv::{
    func_counter: self.func_counter,
    func_fvar_counter: 0,
    func_insn_counter: 0,
    func_block_counter: 0,
    func_mem_counter: 0,
    lifted_func_counter: 0,
  }
}

type FreeNames Array[@knf.Name]

fn FreeNames::new() -> FreeNames {
  []
}

fn add(self : FreeNames, name : @knf.Name) -> Unit {
  if not(self._.contains(name)) {
    self._.push(name)
  }
}

fn merge(
  self : CoreEnv,
  ctx : Ctx,
  free_names : FreeNames,
  other : FreeNames
) -> Unit {
  let _ = self
  for name in other._ {
    match ctx.name_ctx.find(name) {
      None => free_names.add(name)
      _ => ()
    }
  }
}

fn add_insn(self : CoreEnv, func : Fn, bb : BasicBlock, insn : Insn) -> Value {
  let insn_ref = InsnRef({ id: self.func_insn_counter })
  self.func_insn_counter += 1
  func.insns.set(insn_ref, insn)
  bb.seq.push(insn_ref)
  Value::Insn(insn_ref)
}

fn add_mem(self : CoreEnv, func : Fn, _bb : BasicBlock, mem : Mem) -> MemRef {
  let mem_ref = MemRef({ id: self.func_mem_counter })
  self.func_mem_counter += 1
  func.mems.set(mem_ref, mem)
  mem_ref
}

fn build_func_def(
  self : CoreEnv,
  ctx : Ctx,
  core : Core,
  fd : @knf.FuncDef,
  free_names : FreeNames,
  top : Bool
) -> FnRef!Failure {
  let { name, ty, args, body } = fd
  let mut self = self
  let mut ctx = ctx.enter_func_def()
  let func_ref = FnRef({ id: self.func_counter.val })
  self.func_counter.val += 1
  let subenv = self.inherit()
  self = subenv
  let tys = []
  for i, arg in args {
    let (name, ty) = arg
    let ty = self.lower_type!(ty, false)
    tys.push(ty)
    let val = Value::FnParam(func_ref, FnParamRef(Id::new(i)))
    ctx = ctx.add_local_name_value(name, val)
    ctx = ctx.add_name_ty(name, ty)
  }
  let res_ty = self.lower_type!(ty, false)
  let ty = if top {
    ctx = ctx.add_top_name_value(name, Value::Fn(func_ref))
    ctx = ctx.add_name_ty(name, DirectFn(tys, res_ty))
    Type::DirectFn(tys, res_ty)
  } else {
    ctx = ctx.add_local_name_value(name, Value::Self(func_ref))
    ctx = ctx.add_name_ty(name, ClosureFn(tys, res_ty))
    Type::ClosureFn(tys, res_ty)
  }
  let entry_block_ref = BlockRef({ id: self.func_block_counter })
  let func = Fn::{
    id: func_ref,
    name: name.to_string(),
    ty,
    fvars: [],
    entry: entry_block_ref,
    blocks: MutMap::new(),
    insns: MutMap::new(),
    mems: MutMap::new(),
  }
  // Consider to rename here
  // It shouldn't cause problem before backend code generation
  core.names.set(fd.name.to_string(), func_ref)
  core.funcs.set(func_ref, func)
  let entry_block = self.add_basic_block(func, [])
  let (_, exit_block, exit_val) = self.build_knf_expr!(
    free_names, ctx, core, func, entry_block, body,
  )
  guard exit_block.control == Control::Panic else { fail!("expect panic") }
  exit_block.control = Control::Return([exit_val])
  func_ref
}

// In some cases we get a direct function declared in the top level
// But we expect it to be a closure
// So we need to wrap it with a closure
// The result will be a mem type
fn build_top_level_closure(
  self : CoreEnv,
  ctx : Ctx,
  core : Core,
  func : Fn,
  bb : BasicBlock,
  val : Value
) -> (Ctx, Value)!Failure {
  self.build_closure!(ctx, FreeNames::new(), core, func, bb, val)
}

fn build_closure(
  self : CoreEnv,
  ctx : Ctx,
  free_names : FreeNames,
  core : Core,
  func : Fn,
  bb : BasicBlock,
  val : Value
) -> (Ctx, Value)!Failure {
  match val {
    Value::Fn(fr) => {
      let ty = match core.funcs.get(fr).unwrap().ty {
        DirectFn(x, y) => Type::ClosureFn(x, y)
        ClosureFn(_, _) as t => t
        _ => fail!("not a function")
      }
      let mem = self.add_mem(
        func,
        bb,
        Mem::Heap(ty, Value::Int(1 + free_names._.length())),
      )
      let fill = [Fill::Field(0, val)]
      let mut ctx = ctx
      for i, name in free_names._ {
        let (ctx2, val) = self.build_freevar!(free_names, ctx, func, name)
        ctx = ctx2
        fill.push(Fill::Field(i + 1, val))
      }
      let _ = self.add_insn(func, bb, Insn::Alloc(ty, mem, fill))
      (ctx, Value::Mem(func.id, mem))
    }
    _ => (ctx, val)
  }
}

fn build_freevar!(
  self : CoreEnv,
  free_names : FreeNames,
  ctx : Ctx,
  func : Fn,
  name : @knf.Name
) -> (Ctx, Value)!Failure {
  match ctx.name_ctx.find(name) {
    Some(val) => (ctx, val)
    None => {
      free_names.add(name)
      self.func_fvar_counter += 1
      let ty = match ctx.ty_ctx.find(name) {
        Some(ty) => ty
        None => fail!("\{name} not found in \{ctx}")
      }
      let val = Value::FreeVar(
        ty,
        FnFreeVarRef(
          Id::new(
            // NOTICE: the free variables start from 1 because 0 is reserved for the function ptr
            self.func_fvar_counter,
          ),
        ),
      )
      func.fvars.push(ty)
      let ctx = ctx.add_local_name_value(name, val)
      (ctx, val)
    }
  }
}

fn build_name!(
  self : CoreEnv,
  free_names : FreeNames,
  ctx : Ctx,
  core : Core,
  func : Fn,
  bb : BasicBlock,
  name : @knf.Name
) -> (Ctx, Value)!Failure {
  match ctx.top_name_ctx.find(name) {
    Some(val) => self.build_top_level_closure!(ctx, core, func, bb, val)
    None => self.build_freevar!(free_names, ctx, func, name)
  }
}

fn build_name_without_top_level_closure!(
  self : CoreEnv,
  free_names : FreeNames,
  ctx : Ctx,
  _core : Core,
  func : Fn,
  _bb : BasicBlock,
  name : @knf.Name
) -> (Ctx, Value)!Failure {
  match ctx.top_name_ctx.find(name) {
    Some(val) => (ctx, val)
    None => self.build_freevar!(free_names, ctx, func, name)
  }
}

fn build_names(
  self : CoreEnv,
  free_names : FreeNames,
  ctx : Ctx,
  core : Core,
  func : Fn,
  bb : BasicBlock,
  names : Array[@knf.Name]
) -> (Ctx, Array[Value])!Failure {
  let res = []
  let mut ctx = ctx
  for name in names {
    let (ctx2, val) = self.build_name!(free_names, ctx, core, func, bb, name)
    ctx = ctx2
    res.push(val)
  }
  (ctx, res)
}

fn get_type(
  self : CoreEnv,
  core : Core,
  func : Fn,
  _bb : BasicBlock,
  val : Value
) -> Type!Failure {
  let _ = self
  match val {
    Unit => Type::Unit
    Bool(_) => Type::Bool
    Int(_) => Type::Int
    Double(_) => Type::Double
    Insn(insn) => {
      let insn = match func.insns.get(insn) {
        Some(insn) => insn
        None =>
          fail!("insn \{Show::to_string(insn)} not found in \{func.insns}")
      }
      match insn {
        Op(ty, _, _) => ty
        Apply(ty, _, _) => ty
        Call(ty, _, _) => ty
        ExtCall(ty, _, _) => ty
        Alloc(ty, _, _) => ty
        Load(ty, _) => ty
        Store(_, _, _) => Type::Unit
      }
    }
    GlobalThunk(func) => {
      let func = match core.funcs.get(func) {
        Some(func) => func
        None => fail!("func not found")
      }
      match func.ty {
        DirectFn(_, res_ty) => res_ty
        _ => fail!("not a thunk type")
      }
    }
    FreeVar(ty, _) => ty
    BlockParam(block, bp) => {
      let block = match func.blocks.get(block) {
        Some(block) => block
        _ => fail!("bb not found")
      }
      match block {
        BasicBlock(bb) => bb.params[bp.id]
        Loop(_) => fail!("loop not supported")
      }
    }
    FnParam(funcref, fp) => {
      let func = match core.funcs.get(funcref) {
        Some(func) => func
        None => fail!("func not found")
      }
      match func.ty {
        DirectFn(params, _) => params[fp.id]
        ClosureFn(params, _) => params[fp.id]
        _ => fail!("not a function type")
      }
    }
    Mem(funcref, mem) => {
      let func = match core.funcs.get(funcref) {
        Some(func) => func
        None => fail!("func not found")
      }
      let mem = match func.mems.get(mem) {
        Some(mem) => mem
        None => fail!("mem not found")
      }
      match mem {
        Heap(ty, _) => ty
        Stack(ty, _) => ty
      }
    }
    Fn(func) => {
      let func = match core.funcs.get(func) {
        Some(func) => func
        None => fail!("func not found")
      }
      guard (match func.ty {
        DirectFn(_, _) => true
        _ => false
      }) else {
        fail!("not a direct function")
      }
      func.ty
    }
    Self(func) => {
      let func = match core.funcs.get(func) {
        Some(func) => func
        None => fail!("func not found")
      }
      guard (match func.ty {
        ClosureFn(_, _) => true
        _ => false
      }) else {
        fail!("not a closure")
      }
      func.ty
    }
  }
}

fn get_external_function_type(self : CoreEnv, name : String) -> Type!Failure {
  let name = name.substring(start="minimbt_".length(), end=name.length())
  match @typer.externals().find(name) {
    Some(ty) => self.lower_type!(ty, true)
    None => fail!("unknown external function \{name}")
  }
}

fn lower_type(self : CoreEnv, ty : @knf.Type, native : Bool) -> Type!Failure {
  match ty.repr() {
    Unit => Type::Unit
    Bool => Type::Bool
    Int => Type::Int
    Double => Type::Double
    Tuple(xs) => {
      let tys = []
      for x in xs {
        tys.push(self.lower_type!(x, false))
      }
      Type::Tuple(tys)
    }
    Array(x) => Type::Array(self.lower_type!(x, false))
    Ptr => Type::Ptr
    Fun(xs, x) => {
      let tys = []
      for x in xs {
        tys.push(self.lower_type!(x, false))
      }
      let res_ty = self.lower_type!(x, false)
      if native {
        Type::DirectFn(tys, res_ty)
      } else {
        Type::ClosureFn(tys, res_ty)
      }
    }
    Var(_) => fail!("uninstantiated type variable")
  }
}

fn add_basic_block(
  self : CoreEnv,
  func : Fn,
  params : Array[Type]
) -> BasicBlock {
  let block_ref = BlockRef({ id: self.func_block_counter })
  self.func_block_counter += 1
  let bb = BasicBlock::{
    id: block_ref,
    params,
    seq: [],
    control: Control::Panic,
  }
  func.blocks.set(block_ref, Block::BasicBlock(bb))
  bb
}

fn build_knf_expr(
  self : CoreEnv,
  free_names : FreeNames,
  ctx : Ctx,
  core : Core,
  func : Fn,
  bb : BasicBlock,
  knf : Knf
) -> (Ctx, BasicBlock, Value)!Failure {
  match knf {
    Unit => (ctx, bb, Value::Unit)
    Int(i) => (ctx, bb, Value::Int(i))
    Double(d) => (ctx, bb, Value::Double(d))
    Neg(n) | FNeg(n) => {
      let (ctx, n_val) = self.build_name!(free_names, ctx, core, func, bb, n)
      let n_ty = self.get_type!(core, func, bb, n_val)
      let insn = Insn::Op(n_ty, Op::Neg, [n_val])
      let val = self.add_insn(func, bb, insn)
      (ctx, bb, val)
    }
    Add(n1, n2)
    | FAdd(n1, n2)
    | Sub(n1, n2)
    | FSub(n1, n2) | Mul(n1, n2) | FMul(n1, n2) | Div(n1, n2) | FDiv(n1, n2) => {
      let (ctx, n1_val) = self.build_name!(free_names, ctx, core, func, bb, n1)
      let (ctx, n2_val) = self.build_name!(free_names, ctx, core, func, bb, n2)
      let n1_ty = self.get_type!(core, func, bb, n1_val)
      let n2_ty = self.get_type!(core, func, bb, n2_val)
      guard n1_ty == n2_ty else { fail!("type mismatch \{n1_ty} != \{n2_ty}") }
      let insn = Insn::Op(n1_ty, Op::from_knf(knf).unwrap(), [n1_val, n2_val])
      let val = self.add_insn(func, bb, insn)
      (ctx, bb, val)
    }
    IfEq(n1, n2, e1, e2) | IfLe(n1, n2, e1, e2) => {
      let (ctx, n1_val) = self.build_name!(free_names, ctx, core, func, bb, n1)
      let (ctx, n2_val) = self.build_name!(free_names, ctx, core, func, bb, n2)
      let cond = Insn::Op(Bool, Op::from_knf(knf).unwrap(), [n1_val, n2_val])
      let cond_val = self.add_insn(func, bb, cond)
      let then_bb_s = self.add_basic_block(func, [])
      let else_bb_s = self.add_basic_block(func, [])
      let e1_free_names = FreeNames::new()
      let (ctx1, then_bb, then_val) = self.build_knf_expr!(
        e1_free_names, ctx, core, func, then_bb_s, e1,
      )
      // NOTICE: merge ctx instead of ctx1
      self.merge(ctx, free_names, e1_free_names)
      let e2_free_names = FreeNames::new()
      let (ctx, else_bb, else_val) = self.build_knf_expr!(
        e2_free_names, ctx1, core, func, else_bb_s, e2,
      )
      // NOTICE: merge ctx1 instead of ctx
      self.merge(ctx1, free_names, e2_free_names)
      let then_ty = self.get_type!(core, func, bb, then_val)
      let else_ty = self.get_type!(core, func, bb, else_val)
      guard then_ty == else_ty else { fail!("type mismatch") }
      let res_bb = self.add_basic_block(func, [then_ty])
      let then_cont = Control::Jump(res_bb.id, [then_val])
      let else_cont = Control::Jump(res_bb.id, [else_val])
      then_bb.control = then_cont
      else_bb.control = else_cont
      bb.control = Control::Branch(cond_val, then_bb_s.id, [], else_bb_s.id, [])
      (ctx, res_bb, Value::BlockParam(res_bb.id, BlockParamRef(Id::new(0))))
    }
    Let((name, ty), e1, e2) => {
      let e1_free_names = FreeNames::new()
      let (ctx1, cursor, e1_val) = self.build_knf_expr!(
        e1_free_names, ctx, core, func, bb, e1,
      )
      // NOTICE: merge ctx instead of ctx1
      self.merge(ctx, free_names, e1_free_names)
      let ctx = ctx1.add_local_name_value_ty(
        name,
        e1_val,
        self.lower_type!(ty, false),
      )
      self.build_knf_expr!(free_names, ctx, core, func, cursor, e2)
    }
    Var(name) => {
      let (_, val) = self.build_name!(free_names, ctx, core, func, bb, name)
      (ctx, bb, val)
    }
    LetRec(fd, other) => {
      let sub_free_names = FreeNames::new()
      let fdv = self.build_func_def!(ctx, core, fd, sub_free_names, false)
      self.merge(ctx, free_names, sub_free_names)
      // always generate a closure for inner let rec
      let (ctx, clos) = self.build_closure!(
        ctx,
        sub_free_names,
        core,
        func,
        bb,
        Value::Fn(fdv),
      )
      let ctx = ctx.add_local_name_value_ty(
        fd.name,
        clos,
        self.get_type!(core, func, bb, clos),
      )
      self.build_knf_expr!(free_names, ctx, core, func, bb, other)
    }
    Apply(f, args) => {
      let (ctx, f_val) = self.build_name_without_top_level_closure!(
        free_names, ctx, core, func, bb, f,
      )
      let (ctx, arg_vals) = self.build_names!(
        free_names, ctx, core, func, bb, args,
      )
      let f_ty = self.get_type!(core, func, bb, f_val)
      let insn = match f_ty {
        ClosureFn(_, res_ty) => Insn::Apply(res_ty, f_val, arg_vals)
        DirectFn(_, res_ty) => Insn::Call(res_ty, f_val, arg_vals)
        _ => fail!("not a function: \{f_val} is \{f_ty}")
      }
      let val = self.add_insn(func, bb, insn)
      (ctx, bb, val)
    }
    Tuple(xs) => {
      let (ctx, xs_val) = self.build_names!(free_names, ctx, core, func, bb, xs)
      let tys = []
      for x in xs_val {
        tys.push(self.get_type!(core, func, bb, x))
      }
      let ty = Type::Tuple(tys)
      let mem = self.add_mem(
        func,
        bb,
        Mem::Heap(ty, Value::Int(xs_val.length())),
      )
      let fill = []
      for i, x_val in xs_val {
        fill.push(Fill::Field(i, x_val))
      }
      let _ = self.add_insn(func, bb, Insn::Alloc(ty, mem, fill))
      (ctx, bb, Value::Mem(func.id, mem))
    }
    LetTuple(fields, x, e) => {
      let (ctx, x_val) = self.build_name!(free_names, ctx, core, func, bb, x)
      let mut ctx = ctx
      for i, field in fields {
        let (name, field_ty) = field
        let ty = self.lower_type!(field_ty, false)
        let field_val = self.add_insn(
          func,
          bb,
          Insn::Load(ty, MemAccess::Field(ty, x_val, i)),
        )
        ctx = ctx.add_local_name_value_ty(name, field_val, ty)
      }
      self.build_knf_expr!(free_names, ctx, core, func, bb, e)
    }
    Get(x, idx) => {
      let (ctx, x_val) = self.build_name!(free_names, ctx, core, func, bb, x)
      let (ctx, idx_val) = self.build_name!(
        free_names, ctx, core, func, bb, idx,
      )
      let ty = self.get_type!(core, func, bb, x_val)
      let ty = match ty {
        Array(ty) => ty
        _ => fail!("not an array: \{ty}")
      }
      let val = self.add_insn(
        func,
        bb,
        Insn::Load(ty, MemAccess::Offset(ty, x_val, idx_val)),
      )
      (ctx, bb, val)
    }
    Put(x, idx, y) => {
      let (ctx, x_val) = self.build_name!(free_names, ctx, core, func, bb, x)
      let (ctx, idx_val) = self.build_name!(
        free_names, ctx, core, func, bb, idx,
      )
      let (ctx, y_val) = self.build_name!(free_names, ctx, core, func, bb, y)
      let ty = match self.get_type!(core, func, bb, x_val) {
        Array(ty) => ty
        _ => fail!("not an array")
      }
      let _ = self.add_insn(
        func,
        bb,
        Insn::Store(ty, MemAccess::Offset(ty, x_val, idx_val), y_val),
      )
      (ctx, bb, Value::Unit)
    }
    ExternalArray(_) => fail!("not supported")
    ExternalFunctionApplication(f, args) =>
      match f {
        "minimbt_create_array"
        | "minimbt_create_float_array" | "minimbt_create_ptr_array" => {
          // They are ACTUALLY alloc and store
          let (ctx, arg_vals) = self.build_names!(
            free_names, ctx, core, func, bb, args,
          )
          let ty = match f {
            "minimbt_create_array" => Type::Array(Type::Int)
            "minimbt_create_float_array" => Type::Array(Type::Double)
            "minimbt_create_ptr_array" => {
              let arg_ty = self.get_type!(core, func, bb, arg_vals[1])
              Type::Array(arg_ty)
            }
            _ => fail!("not supported")
          }
          let len = arg_vals[0]
          let init = arg_vals[1]
          let mem = self.add_mem(func, bb, Mem::Heap(ty, len))
          let _ = self.add_insn(
            func,
            bb,
            Insn::Alloc(ty, mem, [Fill::Init(init)]),
          )
          (ctx, bb, Value::Mem(func.id, mem))
        }
        _ => {
          let (ctx, arg_vals) = self.build_names!(
            free_names, ctx, core, func, bb, args,
          )
          let f_ty = self.get_external_function_type!(f)
          let res_ty = match f_ty {
            DirectFn(_, res_ty) => res_ty
            _ => fail!("not a external function")
          }
          let insn = Insn::ExtCall(res_ty, f, arg_vals)
          let val = self.add_insn(func, bb, insn)
          (ctx, bb, val)
        }
      }
  }
}

fn build_top_level(
  self : CoreEnv,
  ctx : Ctx,
  core : Core,
  knf : Knf
) -> Unit!Failure {
  match knf {
    Let((x, ty), e1, e2) => {
      let name = try {
        @knf.Name::from_string!("thunk_" + x.to_string())
      } catch {
        _ => fail!("invalid name")
      }
      let fd = @knf.FuncDef::{ name, ty, args: [], body: e1 }
      let f = self.build_func_def!(ctx, core, fd, FreeNames::new(), true)
      let func = match core.funcs.get(f) {
        Some(func) => func
        None => fail!("func not found")
      }
      let ctx = ctx.add_name_ty(fd.name, func.ty)
      let ctx = ctx.add_top_name_value(fd.name, Value::Fn(f))
      let ty = self.lower_type!(ty, false)
      let ctx = ctx.add_name_ty(x, ty)
      let ctx = ctx.add_top_name_value(x, Value::GlobalThunk(f))
      core.globals.push((x.to_string(), Value::GlobalThunk(f)))
      self.build_top_level!(ctx, core, e2)
    }
    LetRec(fd, other) => {
      let f = self.build_func_def!(ctx, core, fd, FreeNames::new(), true)
      let func = match core.funcs.get(f) {
        Some(func) => func
        None => fail!("func not found")
      }
      let ctx = ctx.add_name_ty(fd.name, func.ty)
      let ctx = ctx.add_top_name_value(fd.name, Value::Fn(f))
      self.build_top_level!(ctx, core, other)
    }
    Unit => ()
    e => {
      let n = @knf.Let(
        (
          try {
            @knf.Name::from_string!("start")
          } catch {
            _ => fail!("invalid name")
          },
          @knf.Type::Unit,
        ),
        e,
        Unit,
      )
      self.build_top_level!(ctx, core, n)
    }
  }
}

pub fn build(self : CoreEnv, knf : Knf) -> Core!Failure {
  let core = {
    funcs: MutMap::new(),
    names: MutMap::new(),
    globals: Array::new(),
  }
  let ctx = Ctx::new()
  let _ = self.build_top_level!(ctx, core, knf)
  core
}
