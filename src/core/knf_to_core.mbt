typealias Knf = @knf.Knf

typealias RenameCtx = @immut/hashmap.T[@knf.Name, @knf.Name]

typealias SymbolCtx = @immut/hashmap.T[@knf.Name, SymbolRef]

typealias FnCtx = @immut/hashmap.T[@knf.Name, FnRef]

typealias FnNameCtx = @immut/hashmap.T[FnRef, @knf.Name]

typealias NameCtx = @immut/hashmap.T[@knf.Name, Value]

typealias TypeCtx = @immut/hashmap.T[@knf.Name, Type]

pub struct CoreEnv {
  mut func_counter : Ref[Int]
  mut func_fvar_counter : Int
  mut func_insn_counter : Int
  mut func_block_counter : Int
  mut func_mem_counter : Int
  mut sym_counter : Int
  tracer : @util.SubTracer
}

pub struct Ctx {
  // A name should always be in ty_ctx
  // It should be in name_ctx if it is a local name
  // Otherwise, it should be a free variable or a top-level name
  // After seeing a free variable, we regard it as a local name
  // Thus, we can re-use its value later
  ty_ctx : TypeCtx // types of names
  name_ctx : NameCtx // local names
  top_fn_ctx : FnCtx // top-level function names
  top_fref_name_ctx : FnNameCtx // top-level function names
  top_sym_ctx : SymbolCtx // top-level thunks names
  rename_ctx : RenameCtx // rename names
} derive(Show)

fn Ctx::new() -> Ctx {
  {
    ty_ctx: TypeCtx::new(),
    name_ctx: NameCtx::new(),
    top_fn_ctx: FnCtx::new(),
    top_fref_name_ctx: FnNameCtx::new(),
    top_sym_ctx: SymbolCtx::new(),
    rename_ctx: RenameCtx::new(),
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
    top_fn_ctx: self.top_fn_ctx,
    top_fref_name_ctx: self.top_fref_name_ctx,
    top_sym_ctx: self.top_sym_ctx,
    rename_ctx: self.rename_ctx,
  }
}

fn Ctx::add_name_ty(self : Ctx, name : @knf.Name, ty : Type) -> Ctx {
  {
    ty_ctx: self.ty_ctx.add(name, ty),
    name_ctx: self.name_ctx,
    top_fn_ctx: self.top_fn_ctx,
    top_fref_name_ctx: self.top_fref_name_ctx,
    top_sym_ctx: self.top_sym_ctx,
    rename_ctx: self.rename_ctx,
  }
}

fn Ctx::add_local_name_value(self : Ctx, name : @knf.Name, val : Value) -> Ctx {
  {
    ty_ctx: self.ty_ctx,
    name_ctx: self.name_ctx.add(name, val),
    top_fn_ctx: self.top_fn_ctx,
    top_fref_name_ctx: self.top_fref_name_ctx,
    top_sym_ctx: self.top_sym_ctx,
    rename_ctx: self.rename_ctx,
  }
}

fn Ctx::add_top_fn_value(self : Ctx, name : @knf.Name, fref : FnRef) -> Ctx {
  {
    ty_ctx: self.ty_ctx,
    name_ctx: self.name_ctx,
    top_fn_ctx: self.top_fn_ctx.add(name, fref),
    top_fref_name_ctx: self.top_fref_name_ctx.add(fref, name),
    top_sym_ctx: self.top_sym_ctx,
    rename_ctx: self.rename_ctx,
  }
}

fn Ctx::add_top_sym_value(self : Ctx, name : @knf.Name, sym : SymbolRef) -> Ctx {
  {
    ty_ctx: self.ty_ctx,
    name_ctx: self.name_ctx,
    top_fn_ctx: self.top_fn_ctx,
    top_fref_name_ctx: self.top_fref_name_ctx,
    top_sym_ctx: self.top_sym_ctx.add(name, sym),
    rename_ctx: self.rename_ctx,
  }
}

fn Ctx::add_rename(self : Ctx, name : @knf.Name, new_name : @knf.Name) -> Ctx {
  {
    ty_ctx: self.ty_ctx,
    name_ctx: self.name_ctx,
    top_fn_ctx: self.top_fn_ctx,
    top_fref_name_ctx: self.top_fref_name_ctx,
    top_sym_ctx: self.top_sym_ctx,
    rename_ctx: self.rename_ctx.add(name, new_name),
  }
}

// When entering a function definition, we need to create a new local name context
fn Ctx::enter_func_def(self : Ctx) -> Ctx {
  {
    ty_ctx: self.ty_ctx,
    name_ctx: NameCtx::new(),
    top_fn_ctx: self.top_fn_ctx,
    top_fref_name_ctx: self.top_fref_name_ctx,
    top_sym_ctx: self.top_sym_ctx,
    rename_ctx: self.rename_ctx,
  }
}

pub fn CoreEnv::new(tracer : @util.SubTracer) -> CoreEnv {
  CoreEnv::{
    func_counter: Ref::new(0),
    func_fvar_counter: 0,
    func_insn_counter: 0,
    func_block_counter: 0,
    func_mem_counter: 0,
    sym_counter: 0,
    tracer,
  }
}

pub fn inherit(self : CoreEnv) -> CoreEnv {
  CoreEnv::{
    func_counter: self.func_counter,
    func_fvar_counter: 0,
    func_insn_counter: 0,
    func_block_counter: 0,
    func_mem_counter: 0,
    sym_counter: 0,
    tracer: self.tracer,
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
  Value::Insn(iref=insn_ref)
}

fn add_mem(self : CoreEnv, func : Fn, _bb : BasicBlock, mem : Mem) -> MemRef {
  let mem_ref = MemRef({ id: self.func_mem_counter })
  self.func_mem_counter += 1
  func.mems.set(mem_ref, mem)
  mem_ref
}

fn add_symbol(
  self : CoreEnv,
  core : Core,
  thunk : Fn,
  symbol : String,
  ty : Type
) -> SymbolRef {
  let sym_ref = SymbolRef({ id: self.sym_counter })
  self.sym_counter += 1
  core.global_symbols.set(
    sym_ref,
    Symbol::{ id: sym_ref, name: symbol, ty, thunk: thunk.id },
  )
  core.global_names.push((symbol, sym_ref))
  sym_ref
}

fn next_fn_ref(self : CoreEnv) -> FnRef {
  let fref = FnRef({ id: self.func_counter.val })
  self.func_counter.val += 1
  fref
}

fn make_top_func_def_type(self : CoreEnv, fd : @knf.FuncDef) -> Type!Failure {
  let { ty, args, .. } = fd
  let tys = []
  for arg in args {
    let (_, ty) = arg
    let ty = self.lower_type!(ty, false)
    tys.push(ty)
  }
  let res_ty = self.lower_type!(ty, false)
  Type::DirectFn(params=tys, ty=res_ty)
}

fn build_func_def(
  self : CoreEnv,
  ctx : Ctx,
  core : Core,
  fref : FnRef,
  fd : @knf.FuncDef,
  free_names : FreeNames,
  top : Bool
) -> Unit!Failure {
  let { name, ty, args, body } = fd
  let mut self = self
  let mut ctx = ctx.enter_func_def()
  let new_name = name.to_string() + "_" + fref._.id.to_string()
  let new_name = try {
    @types.Name::from_string!(new_name)
  } catch {
    x => fail!("\{Show::to_string(x)}")
  }
  self.func_counter.val += 1
  let subenv = self.inherit()
  self = subenv
  let tys = []
  for i, arg in args {
    let (name, ty) = arg
    let ty = self.lower_type!(ty, false)
    tys.push(ty)
    let val = Value::FnParam(~fref, fpref=FnParamRef(Id::new(i)))
    ctx = ctx.add_local_name_value(name, val)
    ctx = ctx.add_name_ty(name, ty)
  }
  let res_ty = self.lower_type!(ty, false)
  let ty = if top {
    ctx = ctx.add_rename(name, new_name)
    ctx = ctx.add_top_fn_value(new_name, fref)
    ctx = ctx.add_name_ty(new_name, DirectFn(params=tys, ty=res_ty))
    Type::DirectFn(params=tys, ty=res_ty)
  } else {
    ctx = ctx.add_rename(name, new_name)
    ctx = ctx.add_local_name_value(new_name, Value::Self(~fref))
    ctx = ctx.add_name_ty(new_name, ClosureFn(params=tys, ty=res_ty))
    Type::ClosureFn(params=tys, ty=res_ty)
  }
  let entry_block_ref = BlockRef({ id: self.func_block_counter })
  let func = Fn::{
    id: fref,
    name: new_name.to_string(),
    ty,
    fvars: [],
    entry: entry_block_ref,
    blocks: MutMap::new(),
    insns: MutMap::new(),
    mems: MutMap::new(),
  }
  // Consider to rename here
  // It shouldn't cause problem before backend code generation
  // Days later:
  // It has caused problem
  core.names.set(new_name.to_string(), fref)
  core.funcs.set(fref, func)
  let entry_block = self.add_basic_block(func, [])
  let (_, exit_block, exit_val) = self.build_knf_expr!(
    free_names, ctx, core, func, entry_block, body,
  )
  guard exit_block.control == Control::Panic else { fail!("expect panic") }
  exit_block.control = Control::Return(args=[exit_val])
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
  fr : FnRef
) -> (Ctx, Value)!Failure {
  self.build_closure!(ctx, FreeNames::new(), core, func, bb, fr)
}

fn build_closure(
  self : CoreEnv,
  ctx : Ctx,
  free_names : FreeNames,
  core : Core,
  func : Fn,
  bb : BasicBlock,
  fr : FnRef
) -> (Ctx, Value)!Failure {
  let ty = match core.funcs.get(fr).unwrap().ty {
    DirectFn(~params, ~ty) => Type::ClosureFn(~params, ~ty)
    ClosureFn(..) as t => t
    _ => fail!("not a function")
  }
  let mem = self.add_mem(
    func,
    bb,
    Mem::Heap(~ty, len=Value::Int32(val=1 + free_names._.length())),
  )
  let fills = [Fill::Field(index=0, val=Value::Fn(fref=fr))]
  let mut ctx = ctx
  for i, name in free_names._ {
    let (ctx2, val) = self.build_local_or_free_var!(free_names, ctx, func, name)
    ctx = ctx2
    fills.push(Fill::Field(index=i + 1, ~val))
  }
  let _ = self.add_insn(func, bb, Insn::Alloc(~ty, mref=mem, ~fills))
  (ctx, Value::Mem(fref=func.id, mref=mem))
}

fn build_local_or_free_var!(
  self : CoreEnv,
  free_names : FreeNames,
  ctx : Ctx,
  func : Fn,
  name : @knf.Name
) -> (Ctx, Value)!Failure {
  let name = ctx.rename_ctx.find(name).or(name)
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
        fvref=FnFreeVarRef(
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

fn build_symbol_or_var(
  self : CoreEnv,
  free_names : FreeNames,
  ctx : Ctx,
  core : Core,
  func : Fn,
  bb : BasicBlock,
  name : @knf.Name
) -> (Ctx, Value)!Failure {
  let name = ctx.rename_ctx.find(name).or(name)
  match ctx.top_sym_ctx.find(name) {
    Some(sref) => {
      let sym = core.global_symbols.get(sref).unwrap()
      let insn = self.add_insn(func, bb, Insn::LoadSymbol(ty=sym.ty, ~sref))
      (ctx, insn)
    }
    None => self.build_local_or_free_var!(free_names, ctx, func, name)
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
  let name = ctx.rename_ctx.find(name).or(name)
  match ctx.top_fn_ctx.find(name) {
    Some(val) => self.build_top_level_closure!(ctx, core, func, bb, val)
    None => self.build_symbol_or_var!(free_names, ctx, core, func, bb, name)
  }
}

fn build_name_without_top_level_closure!(
  self : CoreEnv,
  free_names : FreeNames,
  ctx : Ctx,
  core : Core,
  func : Fn,
  bb : BasicBlock,
  name : @knf.Name
) -> (Ctx, Value)!Failure {
  let name = ctx.rename_ctx.find(name).or(name)
  match ctx.top_fn_ctx.find(name) {
    Some(val) => (ctx, Value::Fn(fref=val))
    None => self.build_symbol_or_var!(free_names, ctx, core, func, bb, name)
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
  ctx : Ctx,
  core : Core,
  func : Fn,
  _bb : BasicBlock,
  val : Value
) -> Type!Failure {
  let _ = self
  match val {
    // in this stage, core may not hold enough information for fn ref value
    // extract it from type context
    Fn(~fref) => {
      let name = ctx.top_fref_name_ctx.find(fref).unwrap()
      match ctx.ty_ctx.find(name) {
        Some(ty) => ty
        None => fail!("failed to find type for \{fref}")
      }
    }
    _ => val.ty!(core, func)
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
    Int => Type::Int32
    Double => Type::Double
    Tuple(xs) => {
      let tys = []
      for x in xs {
        tys.push(self.lower_type!(x, false))
      }
      Type::Tuple(fields=tys)
    }
    Array(x) => Type::Array(elem=self.lower_type!(x, false))
    Ptr => Type::Ptr
    Fun(xs, x) => {
      let tys = []
      for x in xs {
        tys.push(self.lower_type!(x, false))
      }
      let res_ty = self.lower_type!(x, false)
      if native {
        Type::DirectFn(params=tys, ty=res_ty)
      } else {
        Type::ClosureFn(params=tys, ty=res_ty)
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
  func.blocks.set(block_ref, Block::BasicBlock(~bb))
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
    Int(i) => (ctx, bb, Value::Int32(val=i.to_int()))
    Double(d) => (ctx, bb, Value::Double(val=d))
    Neg(n) | FNeg(n) => {
      let (ctx, n_val) = self.build_name!(free_names, ctx, core, func, bb, n)
      let n_ty = self.get_type!(ctx, core, func, bb, n_val)
      let insn = Insn::Op(ty=n_ty, op=Op::Neg, args=[n_val])
      let val = self.add_insn(func, bb, insn)
      (ctx, bb, val)
    }
    Add(n1, n2)
    | FAdd(n1, n2)
    | Sub(n1, n2)
    | FSub(n1, n2) | Mul(n1, n2) | FMul(n1, n2) | Div(n1, n2) | FDiv(n1, n2) => {
      let (ctx, n1_val) = self.build_name!(free_names, ctx, core, func, bb, n1)
      let (ctx, n2_val) = self.build_name!(free_names, ctx, core, func, bb, n2)
      let n1_ty = self.get_type!(ctx, core, func, bb, n1_val)
      let n2_ty = self.get_type!(ctx, core, func, bb, n2_val)
      guard n1_ty == n2_ty else {
        fail!("type mismatch \{n1_ty} != \{n2_ty} of \{knf}")
      }
      let insn = Insn::Op(
        ty=n1_ty,
        op=Op::from_knf(knf).unwrap(),
        args=[n1_val, n2_val],
      )
      let val = self.add_insn(func, bb, insn)
      (ctx, bb, val)
    }
    IfEq(n1, n2, e1, e2) | IfLe(n1, n2, e1, e2) => {
      let (ctx, n1_val) = self.build_name!(free_names, ctx, core, func, bb, n1)
      let (ctx, n2_val) = self.build_name!(free_names, ctx, core, func, bb, n2)
      let cond = Insn::Op(
        ty=Bool,
        op=Op::from_knf(knf).unwrap(),
        args=[n1_val, n2_val],
      )
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
      let then_ty = self.get_type!(ctx, core, func, bb, then_val)
      let else_ty = self.get_type!(ctx, core, func, bb, else_val)
      guard then_ty == else_ty else { fail!("type mismatch") }
      let res_bb = self.add_basic_block(func, [then_ty])
      let then_cont = Control::Jump(target=res_bb.id, args=[then_val])
      let else_cont = Control::Jump(target=res_bb.id, args=[else_val])
      then_bb.control = then_cont
      else_bb.control = else_cont
      bb.control = Control::Branch(
        cond=cond_val,
        t=then_bb_s.id,
        t_args=[],
        f=else_bb_s.id,
        f_args=[],
      )
      (
        ctx,
        res_bb,
        Value::BlockParam(bref=res_bb.id, bpref=BlockParamRef(Id::new(0))),
      )
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
      let fdv = self.next_fn_ref()
      self.build_func_def!(ctx, core, fdv, fd, sub_free_names, false)
      self.merge(ctx, free_names, sub_free_names)
      // always generate a closure for inner let rec
      let (ctx, clos) = self.build_closure!(
        ctx, sub_free_names, core, func, bb, fdv,
      )
      let ctx = ctx.add_local_name_value_ty(
        fd.name,
        clos,
        self.get_type!(ctx, core, func, bb, clos),
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
      let f_ty = self.get_type!(ctx, core, func, bb, f_val)
      let insn = match f_ty {
        ClosureFn(~ty, ..) => Insn::Apply(~ty, f=f_val, args=arg_vals)
        DirectFn(~ty, ..) =>
          match f_val {
            Value::Fn(~fref) => Insn::Call(~ty, f=fref, args=arg_vals)
            _ => fail!("\{f} not a function: \{f_val}")
          }
        _ => fail!("not a function: \{f_val} is \{f_ty}")
      }
      let val = self.add_insn(func, bb, insn)
      (ctx, bb, val)
    }
    Tuple(xs) => {
      let (ctx, xs_val) = self.build_names!(free_names, ctx, core, func, bb, xs)
      let tys = []
      for x in xs_val {
        tys.push(self.get_type!(ctx, core, func, bb, x))
      }
      let ty = Type::Tuple(fields=tys)
      let mem = self.add_mem(
        func,
        bb,
        Mem::Heap(~ty, len=Value::Int32(val=xs_val.length())),
      )
      let fills = []
      for i, x_val in xs_val {
        fills.push(Fill::Field(index=i, val=x_val))
      }
      let _ = self.add_insn(func, bb, Insn::Alloc(~ty, mref=mem, ~fills))
      (ctx, bb, Value::Mem(fref=func.id, mref=mem))
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
          Insn::Load(~ty, ma=Address::Field(~ty, val=x_val, index=i)),
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
      let ty = self.get_type!(ctx, core, func, bb, x_val)
      let ty = match ty {
        Array(~elem) => elem
        _ => fail!("not an array: \{ty}")
      }
      let val = self.add_insn(
        func,
        bb,
        Insn::Load(~ty, ma=Address::Offset(~ty, val=x_val, offset=idx_val)),
      )
      (ctx, bb, val)
    }
    Put(x, idx, y) => {
      let (ctx, x_val) = self.build_name!(free_names, ctx, core, func, bb, x)
      let (ctx, idx_val) = self.build_name!(
        free_names, ctx, core, func, bb, idx,
      )
      let (ctx, y_val) = self.build_name!(free_names, ctx, core, func, bb, y)
      let ty = match self.get_type!(ctx, core, func, bb, x_val) {
        Array(~elem) => elem
        _ => fail!("not an array")
      }
      let _ = self.add_insn(
        func,
        bb,
        Insn::Store(
          ~ty,
          ma=Address::Offset(~ty, val=x_val, offset=idx_val),
          val=y_val,
        ),
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
            "minimbt_create_array" => Type::Array(elem=Type::Int32)
            "minimbt_create_float_array" => Type::Array(elem=Type::Double)
            "minimbt_create_ptr_array" => {
              let arg_ty = self.get_type!(ctx, core, func, bb, arg_vals[1])
              Type::Array(elem=arg_ty)
            }
            _ => fail!("not supported")
          }
          let len = arg_vals[0]
          let init = arg_vals[1]
          let mem = self.add_mem(func, bb, Mem::Heap(~ty, ~len))
          let _ = self.add_insn(
            func,
            bb,
            Insn::Alloc(~ty, mref=mem, fills=[Fill::Init(val=init)]),
          )
          (ctx, bb, Value::Mem(fref=func.id, mref=mem))
        }
        "minimbt_int_of_float" | "minimbt_truncate" => {
          let (ctx, arg_vals) = self.build_names!(
            free_names, ctx, core, func, bb, args,
          )
          let insn = Insn::Op(
            ty=Type::Int32,
            op=Op::D2I(rm=Rtz),
            args=[arg_vals[0]],
          )
          let val = self.add_insn(func, bb, insn)
          (ctx, bb, val)
        }
        "minimbt_float_of_int" => {
          let (ctx, arg_vals) = self.build_names!(
            free_names, ctx, core, func, bb, args,
          )
          let insn = Insn::Op(ty=Type::Double, op=Op::I2D, args=[arg_vals[0]])
          let val = self.add_insn(func, bb, insn)
          (ctx, bb, val)
        }
        "minimbt_abs_float" => {
          let (ctx, arg_vals) = self.build_names!(
            free_names, ctx, core, func, bb, args,
          )
          let insn = Insn::Op(ty=Type::Double, op=Op::Abs, args=[arg_vals[0]])
          let val = self.add_insn(func, bb, insn)
          (ctx, bb, val)
        }
        "minimbt_sqrt" => {
          let (ctx, arg_vals) = self.build_names!(
            free_names, ctx, core, func, bb, args,
          )
          let insn = Insn::Op(ty=Type::Double, op=Op::Sqrt, args=[arg_vals[0]])
          let val = self.add_insn(func, bb, insn)
          (ctx, bb, val)
        }
        _ => {
          let (ctx, arg_vals) = self.build_names!(
            free_names, ctx, core, func, bb, args,
          )
          let f_ty = self.get_external_function_type!(f)
          let res_ty = match f_ty {
            DirectFn(~ty, ..) => ty
            _ => fail!("not a external function")
          }
          let insn = Insn::ExtCall(ty=res_ty, ext_f=f, args=arg_vals)
          let val = self.add_insn(func, bb, insn)
          (ctx, bb, val)
        }
      }
  }
}

fn update_ctx_for_top_level_recursive_group(
  self : CoreEnv,
  ctx : Ctx,
  defs : Array[@knf.FuncDef]
) -> Ctx!Failure {
  let mut ctx = ctx
  for fdef in defs {
    let fref = self.next_fn_ref()
    let fty = self.make_top_func_def_type!(fdef)
    ctx = ctx.add_name_ty(fdef.name, fty)
    ctx = ctx.add_top_fn_value(fdef.name, fref)
  }
  ctx
}

fn collect_top_level_recursive_group(
  self : CoreEnv,
  ctx : Ctx,
  defs : Array[@knf.FuncDef],
  knf : Knf
) -> Knf!Failure {
  match knf {
    LetRec(fd, other) => {
      defs.push(fd)
      self.collect_top_level_recursive_group!(ctx, defs, other)
    }
    _ => knf
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
      let thunk_name = "T$" + x.to_string()
      let name = try {
        @knf.Name::from_string!(thunk_name)
      } catch {
        _ => fail!("invalid name")
      }
      let fd = @knf.FuncDef::{ name, ty, args: [], body: e1 }
      let fref = self.next_fn_ref()
      self.build_func_def!(ctx, core, fref, fd, FreeNames::new(), true)
      let func = match core.funcs.get(fref) {
        Some(func) => func
        None => fail!("build_top_level: func not found")
      }
      let ctx = ctx.add_name_ty(fd.name, func.ty)
      let ctx = ctx.add_top_fn_value(fd.name, fref)
      let ty = self.lower_type!(ty, false)
      let ctx = ctx.add_name_ty(x, ty)
      let sref = self.add_symbol(core, func, x.to_string(), ty)
      let ctx = ctx.add_top_sym_value(x, sref)
      self.build_top_level!(ctx, core, e2)
    }
    LetRec(_, _) => {
      let rec_group = []
      let knf = self.collect_top_level_recursive_group!(ctx, rec_group, knf)
      let ctx = self.update_ctx_for_top_level_recursive_group!(ctx, rec_group)
      for def in rec_group {
        let fref = ctx.top_fn_ctx.find(def.name).unwrap()
        self.build_func_def!(ctx, core, fref, def, FreeNames::new(), true)
      }
      self.build_top_level!(ctx, core, knf)
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
    global_names: [],
    global_symbols: MutMap::new(),
  }
  let ctx = Ctx::new()
  let _ = self.build_top_level!(ctx, core, knf)
  self.tracer.record("knf_to_core", core.to_pretty_print())
  core
}
