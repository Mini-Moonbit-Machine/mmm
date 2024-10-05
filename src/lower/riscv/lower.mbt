pub enum RegClass {
  X
  F
}

typealias MutMap[K, V] = @hashmap.T[K, V]

pub struct RiscvFnLoweringCtx {
  core : @core.Core
  func : @core.Fn
  reg_classes : Array[RegClass]
  value_ref_count : MutMap[@core.Value, Int]
  value_regs : MutMap[@core.Value, @riscv.AnyReg]
  closure_reg : @riscv.AnyReg
  order : @lower.LoweringOrder
  mut buffer : Array[@riscv.RvAsm]
  ibuffer : Array[@riscv.RvAsm]
}

fn RiscvFnLoweringCtx::ref_fn(
  self : RiscvFnLoweringCtx,
  func : @core.FnRef
) -> @core.Fn {
  self.core.funcs[func].unwrap()
}

fn RiscvFnLoweringCtx::ref_block(
  self : RiscvFnLoweringCtx,
  bref : @core.BlockRef
) -> @core.Block {
  self.func.blocks[bref].unwrap()
}

fn RiscvFnLoweringCtx::ref_insn(
  self : RiscvFnLoweringCtx,
  iref : @core.InsnRef
) -> @core.Insn {
  self.func.insns[iref].unwrap()
}

fn RiscvFnLoweringCtx::ref_mem(
  self : RiscvFnLoweringCtx,
  mref : @core.MemRef
) -> @core.Mem {
  self.func.mems[mref].unwrap()
}

fn RiscvFnLoweringCtx::ref_free_v(
  self : RiscvFnLoweringCtx,
  fvref : @core.FnFreeVarRef
) -> UInt {
  let _ = self
  let id = fvref._.id
  if id < 0 {
    abort("Invalid free var ref")
  }
  id.reinterpret_as_uint()
}

fn RiscvFnLoweringCtx::i64_sext_u64(
  self : RiscvFnLoweringCtx,
  ty : @core.Type,
  u64 : UInt64
) -> Int64 {
  let _ = self
  match ty {
    @core.Type::Int32 => u64.to_uint().reinterpret_as_int().to_int64()
    @core.Type::Int64 => u64.to_int64()
    _ => abort("Invalid type")
  }
}

fn RiscvFnLoweringCtx::i32_as_u64(
  self : RiscvFnLoweringCtx,
  i32 : Int
) -> UInt64 {
  let _ = self
  i32.to_int64().to_uint64()
}

fn RiscvFnLoweringCtx::i64_as_u64(
  self : RiscvFnLoweringCtx,
  i64 : Int64
) -> UInt64 {
  let _ = self
  i64.to_uint64()
}

fn RiscvFnLoweringCtx::f64_as_u64(
  self : RiscvFnLoweringCtx,
  i : Double
) -> UInt64 {
  let _ = self
  i.reinterpret_as_u64()
}

fn RiscvFnLoweringCtx::arg2(
  self : RiscvFnLoweringCtx,
  args : Array[@core.Value]
) -> (@core.Value, @core.Value)? {
  let _ = self
  match args {
    [a, b] => Some((a, b))
    _ => None
  }
}

fn RiscvFnLoweringCtx::arg1(
  self : RiscvFnLoweringCtx,
  args : Array[@core.Value]
) -> @core.Value? {
  let _ = self
  match args {
    [a] => Some(a)
    _ => None
  }
}

fn RiscvFnLoweringCtx::next_any_reg(
  reg_classes : Array[RegClass],
  reg_class : RegClass
) -> @riscv.AnyReg {
  let id = reg_classes.length()
  reg_classes.push(reg_class)
  id
}

fn RiscvFnLoweringCtx::next_reg(
  self : RiscvFnLoweringCtx,
  reg_class : RegClass
) -> @riscv.AnyReg {
  RiscvFnLoweringCtx::next_any_reg(self.reg_classes, reg_class)
}

fn RiscvFnLoweringCtx::next_virtual_xreg(
  self : RiscvFnLoweringCtx
) -> @riscv.Reg {
  V(id=RiscvFnLoweringCtx::next_any_reg(self.reg_classes, RegClass::X)._)
}

fn RiscvFnLoweringCtx::next_virtual_freg(
  self : RiscvFnLoweringCtx
) -> @riscv.FReg {
  Fv(id=RiscvFnLoweringCtx::next_any_reg(self.reg_classes, RegClass::F)._)
}

fn RiscvFnLoweringCtx::emit_risc_v(
  self : RiscvFnLoweringCtx,
  insn : @riscv.RvAsm
) -> Unit {
  self.ibuffer.push(insn)
}

fn RiscvFnLoweringCtx::xreg_new(
  self : RiscvFnLoweringCtx,
  r : @riscv.AnyReg
) -> @riscv.Reg {
  let _ = self
  let id = r._
  V(~id)
}

fn RiscvFnLoweringCtx::freg_new(
  self : RiscvFnLoweringCtx,
  r : @riscv.AnyReg
) -> @riscv.FReg {
  let _ = self
  let id = r._
  Fv(~id)
}

fn RiscvFnLoweringCtx::xreg_to_reg(
  self : RiscvFnLoweringCtx,
  r : @riscv.Reg
) -> @riscv.AnyReg {
  let _ = self
  match r {
    V(~id) => id
    _ => abort("Not a virtual register")
  }
}

fn RiscvFnLoweringCtx::freg_to_reg(
  self : RiscvFnLoweringCtx,
  r : @riscv.FReg
) -> @riscv.AnyReg {
  let _ = self
  match r {
    Fv(~id) => id
    _ => abort("Not a virtual register")
  }
}

fn RiscvFnLoweringCtx::put_in_reg(
  self : RiscvFnLoweringCtx,
  v : @core.Value
) -> @riscv.AnyReg {
  let r = self.value_regs[v].unwrap()
  self.value_ref_count[v] = self.value_ref_count[v].unwrap() + 1
  r
}

fn RiscvFnLoweringCtx::is_zero_reg(
  self : RiscvFnLoweringCtx,
  r : @riscv.Reg
) -> Unit? {
  let _ = self
  if r == Zero {
    Some(())
  } else {
    None
  }
}

fn RiscvFnLoweringCtx::zero_reg(self : RiscvFnLoweringCtx) -> @riscv.Reg {
  let _ = self
  Zero
}

fn RiscvFnLoweringCtx::is_closure_reg(
  self : RiscvFnLoweringCtx,
  r : @riscv.Reg
) -> Unit? {
  match r {
    V(~id) => if id == self.closure_reg._ { Some(()) } else { None }
    _ => None
  }
}

fn RiscvFnLoweringCtx::closure_reg(self : RiscvFnLoweringCtx) -> @riscv.Reg {
  V(id=self.closure_reg._)
}

let imm_min : Int64 = -(1).to_int64().lsl(19).lsr(12) - (1).to_int64().lsl(11)

let imm_max : Int64 = ((1).to_int64().lsl(19) - 1).lsr(12) +
  ((1).to_int64().lsl(11) - 1)

fn RiscvFnLoweringCtx::i64_generate_imm(
  self : RiscvFnLoweringCtx,
  i64 : Int64
) -> (@riscv.Imm20, @riscv.Imm12)? {
  let _ = self
  if i64 >= -2048 && i64 <= 2047 {
    Some((0, i64.land(0xfff).to_int()))
  } else if i64 >= imm_min && i64 <= imm_max {
    None
  } else if i64 > 0 {
    let mut imm20 = i64 / 4096
    let mut imm12 = i64 % 4096
    if imm12 >= 2048 {
      imm12 -= 4096
      imm20 += 1
    }
    let imm20 = imm20.to_int()
    let imm12 = imm12.to_int()
    Some((imm20, imm12))
  } else {
    let i64 = i64.abs()
    let mut imm20 = i64 / 4096
    let mut imm12 = i64 % 4096
    if imm12 < -2048 {
      imm12 += 4096
      imm20 -= 1
    }
    let imm20 = imm20.to_int()
    let imm12 = imm12.to_int()
    Some((imm20, imm12))
  }
}

fn RiscvFnLoweringCtx::i32_generate_imm(
  self : RiscvFnLoweringCtx,
  i32 : Int
) -> (@riscv.Imm20, @riscv.Imm12)? {
  self.i64_generate_imm(i32.to_int64())
}

fn RiscvFnLoweringCtx::imm20_is_zero(
  self : RiscvFnLoweringCtx,
  imm20 : @riscv.Imm20
) -> Unit? {
  let _ = self
  if imm20._ == 0 {
    Some(())
  } else {
    None
  }
}

fn RiscvFnLoweringCtx::imm12_is_zero(
  self : RiscvFnLoweringCtx,
  imm12 : @riscv.Imm12
) -> Unit? {
  let _ = self
  if imm12._ == 0 {
    Some(())
  } else {
    None
  }
}

fn RiscvFnLoweringCtx::imm20_to_i32(
  self : RiscvFnLoweringCtx,
  imm20 : @riscv.Imm20
) -> Int {
  let _ = self
  imm20._
}

fn RiscvFnLoweringCtx::imm12_to_i32(
  self : RiscvFnLoweringCtx,
  imm12 : @riscv.Imm12
) -> Int {
  let _ = self
  imm12._
}

fn RiscvFnLoweringCtx::imm32_to_string(
  self : RiscvFnLoweringCtx,
  i32 : Int
) -> String {
  let _ = self
  "\{i32}"
}

fn RiscvFnLoweringCtx::imm64_to_string(
  self : RiscvFnLoweringCtx,
  i64 : Int64
) -> String {
  let _ = self
  "\{i64}"
}

fn RiscvFnLoweringCtx::lower_call(
  self : RiscvFnLoweringCtx,
  ty : @core.Type,
  f : @core.Fn,
  args : Array[@core.Value]
) -> @riscv.AnyReg? {
  None
}

fn RiscvFnLoweringCtx::lower_ext_call(
  self : RiscvFnLoweringCtx,
  ty : @core.Type,
  f : String,
  args : Array[@core.Value]
) -> @riscv.AnyReg? {
  None
}

fn RiscvFnLoweringCtx::lower_apply(
  self : RiscvFnLoweringCtx,
  ty : @core.Type,
  f : @core.Value,
  args : Array[@core.Value]
) -> @riscv.AnyReg? {
  None
}

fn RiscvFnLoweringCtx::imm12_from_val(
  self : RiscvFnLoweringCtx,
  v : @core.Value
) -> @riscv.Imm12? {
  let imm = match v {
    Unit => Some((@riscv.Imm20(0), @riscv.Imm12(0)))
    Bool(val=true) => Some((@riscv.Imm20(0), @riscv.Imm12(1)))
    Bool(val=false) => Some((@riscv.Imm20(0), @riscv.Imm12(0)))
    Int32(~val) => self.i32_generate_imm(val)
    Int64(~val) => self.i64_generate_imm(val)
    _ => None
  }
  match imm {
    Some((imm20, imm12)) => if imm20._ == 0 { Some(imm12) } else { None }
    None => None
  }
}

fn ty_to_reg_class(ty : @core.Type) -> RegClass {
  match ty {
    @core.Type::Double => F
    _ => X
  }
}

fn RiscvFnLoweringCtx::assign_regs_for_mem(self : RiscvFnLoweringCtx) -> Unit {
  for mem in self.func.mems {
    let (mref, _mem) = mem
    let mval = @core.Value::Mem(fref=self.func.id, ~mref)
    let reg = self.next_reg(X)
    self.value_regs.set(mval, reg)
  }
}

fn RiscvFnLoweringCtx::assign_regs_for_free_variables(
  self : RiscvFnLoweringCtx
) -> Unit {
  self.value_regs.set(@core.Value::Self(fref=self.func.id), self.closure_reg)
  for id, fv_type in self.func.fvars {
    let id = id + 1 // 0 is reserved for the closure pointer
    let reg = self.next_reg(ty_to_reg_class(fv_type))
    self.value_regs.set(@core.Value::FreeVar(ty=fv_type, fvref={ id, }), reg)
  }
}

fn RiscvFnLoweringCtx::finalize_insn(self : RiscvFnLoweringCtx) -> Unit {
  self.ibuffer.rev_inplace()
  self.buffer.append(self.ibuffer)
  self.ibuffer.clear()
}

fn RiscvFnLoweringCtx::finalize_basic_block(
  self : RiscvFnLoweringCtx
) -> Array[@riscv.RvAsm] {
  self.buffer.rev_inplace()
  let old = self.buffer
  self.buffer = []
  old
}

fn RiscvFnLoweringCtx::assign_regs_for_block_params_and_instrs(
  self : RiscvFnLoweringCtx
) -> Unit {
  let order = self.order.order
  fn assign(bb : @core.BasicBlock) {
    for id, param in bb.params {
      let reg = self.next_reg(ty_to_reg_class(param))
      self.value_regs.set(
        @core.Value::BlockParam(bref=bb.id, bpref={ id, }),
        reg,
      )
    }
    for iref in bb.seq {
      let ty = self.ref_insn(iref).ty()
      let reg = self.next_reg(ty_to_reg_class(ty))
      self.value_regs.set(@core.Value::Insn(~iref), reg)
    }
  }

  for block in order {
    match block {
      BasicBlock(~bb) => assign(bb)
      _ => () // skip critical edges
    }
  }
}

fn RiscvFnLoweringCtx::assign_regs_for_function_parameters(
  self : RiscvFnLoweringCtx
) -> Unit {
  fn assign_with_ty(id : Int, ty : @core.Type) {
    let reg = self.next_reg(ty_to_reg_class(ty))
    self.value_regs.set(
      @core.Value::FnParam(fref=self.func.id, fpref={ id, }),
      reg,
    )
  }

  match self.func.ty {
    @core.Type::ClosureFn(~params, ..) | @core.Type::DirectFn(~params, ..) =>
      for i, param in params {
        assign_with_ty(i, param)
      }
    _ => abort("Invalid function type")
  }
}

pub fn RiscvFnLoweringCtx::new(
  core : @core.Core,
  func : @core.Fn
) -> RiscvFnLoweringCtx {
  let value_ref_count = MutMap::new()
  let value_regs = MutMap::new()
  // v0 is reserved for the closure pointer
  // we will set it up in assign_regs_for_free_variables
  // NOTE: fv0 is considered to be the closure pointer
  let reg_classes = [X]
  let closure_reg = @riscv.AnyReg(0)
  let order = @lower.LoweringOrder::new(func)
  let lower_ctx = RiscvFnLoweringCtx::{
    core,
    func,
    reg_classes,
    value_ref_count,
    value_regs,
    closure_reg,
    order,
    buffer: [],
    ibuffer: [],
  }
  lower_ctx.assign_regs_for_function_parameters()
  lower_ctx.assign_regs_for_free_variables()
  lower_ctx.assign_regs_for_mem()
  lower_ctx.assign_regs_for_block_params_and_instrs()
  lower_ctx
}
