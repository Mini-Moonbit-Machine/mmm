pub enum RegClass {
  X
  F
}

typealias MutMap[K, V] = @hashmap.T[K, V]

typealias MutSet[K] = @hashset.T[K]

fn abort[T](msg : String) -> T {
  println(msg)
  panic()
}

let trace_flag : Bool = false

fn trace(msg : String) -> Unit {
  if trace_flag {
    println(msg)
  }
}

pub struct RiscvFnLoweringCtx {
  core : @core.Core
  func : @core.Fn
  reg_classes : Array[RegClass]
  value_ref_count : MutMap[@core.Value, Int]
  value_regs : MutMap[@core.Value, @riscv.AnyReg]
  closure_reg : @riscv.AnyReg
  alloc_reg : @riscv.AnyReg
  order : @lower.LoweringOrder
  mut buffer : Array[MInsn]
  ibuffer : Array[MInsn]
  ialias : Array[@riscv.AnyReg]
}

fn RiscvFnLoweringCtx::value_regs_to_string(
  self : RiscvFnLoweringCtx
) -> String {
  fn pretty_reg(kv : (@core.Value, @riscv.AnyReg)) -> String {
    let xreg = kv.1.virt_to_phy()
    let freg = kv.1.virt_f_to_phy()
    match (xreg, freg) {
      (Some(x), None) => "\{kv.0} -> \{x}"
      (None, Some(f)) => "\{kv.0} -> \{f}"
      _ => "\{kv.0} -> \{kv.1}"
    }
  }

  let mut first = true
  let mut str = ""
  for kv in self.value_regs {
    if first {
      first = false
    } else {
      str += ", "
    }
    str += pretty_reg(kv)
  }
  str
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

fn RiscvFnLoweringCtx::ref_symbol(
  self : RiscvFnLoweringCtx,
  sref : @core.SymbolRef
) -> @core.Symbol {
  self.core.global_symbols[sref].unwrap()
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
  @riscv.AnyReg::{ reg: id }
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
  V(RiscvFnLoweringCtx::next_any_reg(self.reg_classes, RegClass::X))
}

fn RiscvFnLoweringCtx::next_virtual_freg(
  self : RiscvFnLoweringCtx
) -> @riscv.FReg {
  Fv(RiscvFnLoweringCtx::next_any_reg(self.reg_classes, RegClass::F))
}

fn RiscvFnLoweringCtx::emit_risc_v(
  self : RiscvFnLoweringCtx,
  insn : MInsn
) -> Unit {
  self.ibuffer.push(insn)
}

fn xreg(r : @riscv.AnyReg) -> @riscv.Reg {
  r.virt_to_phy().or_else(fn() { abort("Invalid register") })
}

fn freg(r : @riscv.AnyReg) -> @riscv.FReg {
  r.virt_f_to_phy().or_else(fn() { abort("Invalid register") })
}

fn RiscvFnLoweringCtx::xreg_new(
  self : RiscvFnLoweringCtx,
  r : @riscv.AnyReg
) -> @riscv.Reg {
  let _ = self
  r |> xreg
}

fn RiscvFnLoweringCtx::freg_new(
  self : RiscvFnLoweringCtx,
  r : @riscv.AnyReg
) -> @riscv.FReg {
  let _ = self
  r |> freg
}

fn xreg_to(r : @riscv.Reg) -> @riscv.AnyReg {
  r.phy_to_virt()
}

fn freg_to(r : @riscv.FReg) -> @riscv.AnyReg {
  r.phy_to_virt_f()
}

fn RiscvFnLoweringCtx::xreg_to_reg(
  self : RiscvFnLoweringCtx,
  r : @riscv.Reg
) -> @riscv.AnyReg {
  let _ = self
  r |> xreg_to
}

fn RiscvFnLoweringCtx::freg_to_reg(
  self : RiscvFnLoweringCtx,
  r : @riscv.FReg
) -> @riscv.AnyReg {
  let _ = self
  r |> freg_to
}

fn RiscvFnLoweringCtx::put_in_reg(
  self : RiscvFnLoweringCtx,
  v : @core.Value
) -> @riscv.AnyReg {
  let r = self.value_regs[v].or_else(
    fn() { abort("No reg assigned for value \{v}") },
  )
  self.value_ref_count[v] = self.value_ref_count[v].or_else(
      fn() { abort("No ref count assigned for value \{v}") },
    ) +
    1
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

fn RiscvFnLoweringCtx::is_return_reg(
  self : RiscvFnLoweringCtx,
  r : @riscv.Reg
) -> Unit? {
  let _ = self
  if r == @riscv.reg_ret {
    Some(())
  } else {
    None
  }
}

fn RiscvFnLoweringCtx::return_reg(self : RiscvFnLoweringCtx) -> @riscv.Reg {
  let _ = self
  @riscv.reg_ret
}

fn RiscvFnLoweringCtx::values_to_args(
  self : RiscvFnLoweringCtx,
  values : Array[@core.Value]
) -> @generated.Args? {
  let _ = self
  let x = match values {
    [] => @generated.Args::Args0
    [arg1] => @generated.Args::Args1(~arg1)
    [arg1, arg2] => @generated.Args::Args2(~arg1, ~arg2)
    _ => return None
  }
  Some(x)
}

fn RiscvFnLoweringCtx::is_closure_reg(
  self : RiscvFnLoweringCtx,
  r : @riscv.Reg
) -> Unit? {
  match r {
    V(r) => if r == self.closure_reg { Some(()) } else { None }
    _ => None
  }
}

fn RiscvFnLoweringCtx::closure_reg(self : RiscvFnLoweringCtx) -> @riscv.Reg {
  V(self.closure_reg)
}

let imm_min : Int64 = -(1).to_int64().lsl(19).lsr(12) - (1).to_int64().lsl(11)

let imm_max : Int64 = ((1).to_int64().lsl(19) - 1).lsr(12) +
  ((1).to_int64().lsl(11) - 1)

fn RiscvFnLoweringCtx::i64_generate_imm12(
  self : RiscvFnLoweringCtx,
  i64 : Int64
) -> @riscv.Imm12? {
  let _ = self
  if i64 >= -2048 && i64 <= 2047 {
    Some(i64.land(0xfff).to_int())
  } else {
    None
  }
}

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

fn RiscvFnLoweringCtx::i32_generate_imm12(
  self : RiscvFnLoweringCtx,
  i32 : Int
) -> @riscv.Imm12? {
  self.i64_generate_imm12(i32.to_int64())
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

fn RiscvFnLoweringCtx::copy_call_arguments(
  self : RiscvFnLoweringCtx,
  args : Array[@core.Value]
) -> Unit {
  let mut used_xarg = 0
  let mut used_farg = 0
  for arg in args {
    let ty = self.type_of(arg)
    let argc = ty_to_reg_class(ty)
    let r = @generated.constructor_lower_value(self, arg)
    match argc {
      X => {
        @generated.constructor_rv_mv_to(
          self,
          @riscv.reg_arg_list[used_xarg],
          xreg(r),
        )
        used_xarg += 1
        if used_xarg >= @riscv.reg_arg_list.length() {
          abort("Too many arguments")
        }
      }
      F => {
        @generated.constructor_rv_f_mv_to(
          self,
          @riscv.freg_arg_list[used_farg],
          freg(r),
        )
        used_farg += 1
        if used_farg >= @riscv.freg_arg_list.length() {
          abort("Too many arguments")
        }
      }
    }
  }
}

fn RiscvFnLoweringCtx::copy_return_value(
  self : RiscvFnLoweringCtx,
  ty : @core.Type
) -> @riscv.AnyReg {
  match ty_to_reg_class_opt(ty) {
    Some(X) => xreg_to(@generated.constructor_rv_mv(self, @riscv.reg_ret))
    Some(F) => freg_to(@generated.constructor_rv_f_mv_d(self, @riscv.freg_ret))
    None => xreg_to(@generated.constructor_rv_mv(self, self.zero_reg()))
  }
}

fn RiscvFnLoweringCtx::lower_call(
  self : RiscvFnLoweringCtx,
  ty : @core.Type,
  f : @core.Fn,
  args : Array[@core.Value]
) -> @riscv.AnyReg {
  self.copy_call_arguments(args)
  @generated.constructor_rv_call(self, Label(f.name.to_string()))
  self.copy_return_value(ty)
}

fn RiscvFnLoweringCtx::lower_ext_call(
  self : RiscvFnLoweringCtx,
  ty : @core.Type,
  f : String,
  args : Array[@core.Value]
) -> @riscv.AnyReg {
  self.copy_call_arguments(args)
  @generated.constructor_rv_call(self, Label(f))
  self.copy_return_value(ty)
}

fn RiscvFnLoweringCtx::lower_apply(
  self : RiscvFnLoweringCtx,
  ty : @core.Type,
  f : @core.Value,
  args : Array[@core.Value]
) -> @riscv.AnyReg {
  let closure_addr = @generated.constructor_lower_value(self, f)
  @generated.constructor_rv_mv_to(self, @riscv.reg_closure, xreg(closure_addr))
  let func_ma : @riscv.MemAccess[@riscv.Reg, @riscv.Imm12] = {
    base: xreg(closure_addr),
    offset: 0,
  }
  let fp = @generated.constructor_rv_ld(self, func_ma)
  self.copy_call_arguments(args)
  @generated.constructor_rv_jalr(self, fp)
  self.copy_return_value(ty)
}

fn RiscvFnLoweringCtx::lower_load_fn(
  self : RiscvFnLoweringCtx,
  func : @core.FnRef
) -> @riscv.AnyReg {
  @generated.constructor_rv_la(self, self.ref_fn(func).name) |> xreg_to
}

fn RiscvFnLoweringCtx::lower_load_symbol(
  self : RiscvFnLoweringCtx,
  ty : @core.Type,
  sym : @core.SymbolRef
) -> @riscv.AnyReg {
  match ty_to_reg_class_opt(ty) {
    Some(c) => {
      let addr = @generated.constructor_rv_la(self, self.ref_symbol(sym).name)
      let ma : @riscv.MemAccess[@riscv.Reg, @riscv.Imm12] = {
        base: addr,
        offset: 0,
      }
      match (ty.size(), c) {
        (4, X) => xreg_to(@generated.constructor_rv_lw(self, ma))
        (8, X) => xreg_to(@generated.constructor_rv_ld(self, ma))
        (8, F) => freg_to(@generated.constructor_rv_fld(self, ma))
        _ => abort("Invalid type size")
      }
    }
    None => xreg_to(@generated.constructor_rv_mv(self, self.zero_reg()))
  }
}

fn RiscvFnLoweringCtx::lower_address(
  self : RiscvFnLoweringCtx,
  addr : @core.Address
) -> @riscv.MemAccess[@riscv.Reg, @riscv.Imm12] {
  match addr {
    Field(~ty, ~val, ~index) => {
      let size = ty.size()
      let base = @generated.constructor_lower_value(self, val)
      let offset = index * size
      let imm = self
        .i32_generate_imm(offset)
        .or_else(fn() { abort("offset too large") })
      if imm.0._ != 0 {
        let imm_in_reg = @generated.constructor_lower_imm(
          self,
          @core.Type::Int32,
          offset.to_int64().to_uint64(),
        )
        let offset = @generated.constructor_rv_add(
          self,
          xreg(base),
          xreg(imm_in_reg),
        )
        let ma : @riscv.MemAccess[@riscv.Reg, @riscv.Imm12] = {
          base: offset,
          offset: 0,
        }
        ma
      } else {
        let ma : @riscv.MemAccess[@riscv.Reg, @riscv.Imm12] = {
          base: xreg(base),
          offset: index * size,
        }
        ma
      }
    }
    Offset(~ty, ~val, ~offset) => {
      let size = ty.size()
      let base = @generated.constructor_lower_value(self, val)
      let index = @generated.constructor_lower_value(self, offset)
      let offset = match size {
        4 => @generated.constructor_rv_slli(self, xreg(index), 2)
        8 => @generated.constructor_rv_slli(self, xreg(index), 3)
        _ => abort("Invalid type size")
      }
      let addr = @generated.constructor_rv_add(self, xreg(base), offset)
      let ma : @riscv.MemAccess[@riscv.Reg, @riscv.Imm12] = {
        base: addr,
        offset: 0,
      }
      ma
    }
  }
}

fn RiscvFnLoweringCtx::lower_store(
  self : RiscvFnLoweringCtx,
  ty : @core.Type,
  addr : @core.Address,
  val : @core.Value
) -> @riscv.AnyReg {
  let r = @generated.constructor_lower_value(self, val)
  match ty_to_reg_class_opt(ty) {
    Some(c) => {
      let ma = self.lower_address(addr)
      match (ty.size(), c) {
        (4, X) => @generated.constructor_rv_sw(self, xreg(r), ma)
        (8, X) => @generated.constructor_rv_sd(self, xreg(r), ma)
        (8, F) => @generated.constructor_rv_fsd(self, freg(r), ma)
        _ => abort("Invalid type size")
      }
    }
    None => ()
  }
  self.zero_reg() |> xreg_to
}

fn RiscvFnLoweringCtx::lower_load(
  self : RiscvFnLoweringCtx,
  ty : @core.Type,
  addr : @core.Address
) -> @riscv.AnyReg {
  match ty_to_reg_class_opt(ty) {
    Some(c) => {
      let ma = self.lower_address(addr)
      match (ty.size(), c) {
        (4, X) => xreg_to(@generated.constructor_rv_lw(self, ma))
        (8, X) => xreg_to(@generated.constructor_rv_ld(self, ma))
        (8, F) => freg_to(@generated.constructor_rv_fld(self, ma))
        _ => abort("Invalid type size")
      }
    }
    None => xreg_to(@generated.constructor_rv_mv(self, self.zero_reg()))
  }
}

fn RiscvFnLoweringCtx::do_fill_with_memset_set_reg_length(
  self : RiscvFnLoweringCtx,
  len : @riscv.Reg
) -> Unit {
  let _ = self
  @generated.constructor_rv_mv_to(self, @riscv.A1, len)
}

fn RiscvFnLoweringCtx::do_fill_with_memset_set_imm_length(
  self : RiscvFnLoweringCtx,
  len : Int
) -> Unit {
  let _ = self
  let len = @generated.constructor_rv_li(self, len.to_string())
  self.do_fill_with_memset_set_reg_length(len)
}

fn RiscvFnLoweringCtx::do_fill_with_memset(
  self : RiscvFnLoweringCtx,
  fill : @core.Fill,
  ptr : @riscv.AnyReg,
  elem_sz : Int,
  init_rc : RegClass,
  init_reg : @riscv.AnyReg
) -> Unit {
  match fill {
    Init(..) =>
      match init_rc {
        X => {
          @generated.constructor_rv_mv_to(self, @riscv.reg_ret, xreg(ptr))
          // A1 is assumed to be the length of the buffer
          @generated.constructor_rv_mv_to(self, @riscv.A2, xreg(init_reg))
          @generated.constructor_rv_call(self, Label("_memseti32"))
        }
        F => {
          @generated.constructor_rv_mv_to(self, @riscv.reg_ret, xreg(ptr))
          // A1 is assumed to be the length of the buffer
          @generated.constructor_rv_f_mv_to(self, @riscv.Fa0, freg(init_reg))
          @generated.constructor_rv_call(self, Label("_memsetf64"))
        }
      }
    Zero => {
      @generated.constructor_rv_mv_to(self, @riscv.reg_ret, xreg(ptr))
      @generated.constructor_rv_call(self, Label("_memset0e\{elem_sz}"))
    }
    _ => abort("Invalid fill")
  }
}

fn RiscvFnLoweringCtx::do_fill_at_small_offset(
  self : RiscvFnLoweringCtx,
  ptr : @riscv.AnyReg,
  elem_sz : Int,
  index : @riscv.Imm12,
  init_rc : RegClass,
  init_reg : @riscv.AnyReg
) -> Unit {
  let base = xreg(ptr)
  let offset = index
  match (elem_sz, init_rc) {
    (4, X) =>
      @generated.constructor_rv_sw(self, xreg(init_reg), { base, offset })
    (8, X) =>
      @generated.constructor_rv_sd(self, xreg(init_reg), { base, offset })
    (8, F) =>
      @generated.constructor_rv_fsd(self, freg(init_reg), { base, offset })
    _ => abort("Invalid type size")
  }
}

fn RiscvFnLoweringCtx::get_init_reg_with_class(
  self : RiscvFnLoweringCtx,
  fill : @core.Fill
) -> (RegClass, @riscv.AnyReg) {
  match fill {
    Zero => (X, self.zero_reg() |> xreg_to)
    Init(~val) | Field(~val, ..) => {
      let rc = ty_to_reg_class(self.type_of(val))
      let reg = @generated.constructor_lower_value(self, val)
      (rc, reg)
    }
  }
}

fn RiscvFnLoweringCtx::do_fill_field(
  self : RiscvFnLoweringCtx,
  fill : @core.Fill,
  ptr : @riscv.AnyReg,
  elem_sz : Int,
  index : Int
) -> Unit {
  let (init_rc, init_reg) = self.get_init_reg_with_class(fill)
  let imm_offset = self.i32_generate_imm12(index * elem_sz)
  match imm_offset {
    Some(imm12) =>
      self.do_fill_at_small_offset(ptr, elem_sz, imm12, init_rc, init_reg)
    None => abort("Field offset too large")
  }
}

fn RiscvFnLoweringCtx::do_fill(
  self : RiscvFnLoweringCtx,
  fill : @core.Fill,
  ptr : @riscv.AnyReg,
  elem_sz : Int,
  imm_len : Int?,
  reg_len : @riscv.AnyReg?
) -> Unit {
  match imm_len {
    Some(len) =>
      match fill {
        Zero | Init(..) => {
          let (init_rc, init_reg) = self.get_init_reg_with_class(fill)
          if len * elem_sz <= 256 {
            for i in 0..<len {
              // i * elem_sz is guaranteed to be an imm12
              self.do_fill_at_small_offset(
                ptr,
                elem_sz,
                i * elem_sz,
                init_rc,
                init_reg,
              )
            }
          } else {
            self.do_fill_with_memset_set_imm_length(len)
            self.do_fill_with_memset(fill, ptr, elem_sz, init_rc, init_reg)
          }
        }
        Field(~index, ..) => self.do_fill_field(fill, ptr, elem_sz, index)
      }
    None =>
      match reg_len {
        Some(reg) =>
          match fill {
            Zero | Init(..) => {
              let (init_rc, init_reg) = self.get_init_reg_with_class(fill)
              self.do_fill_with_memset_set_reg_length(xreg(reg))
              self.do_fill_with_memset(fill, ptr, elem_sz, init_rc, init_reg)
            }
            Field(~index, ..) => self.do_fill_field(fill, ptr, elem_sz, index)
          }
        None => abort("Invalid length")
      }
  }
}

fn RiscvFnLoweringCtx::do_alloc_and_fill(
  self : RiscvFnLoweringCtx,
  mref : @core.MemRef,
  fills : @core.Fills
) -> @riscv.AnyReg {
  let mem = self.ref_mem(mref)
  match mem {
    Stack(..) => abort("Stack allocation not supported")
    Heap(~len, ~ty) => {
      let elem_sz = ty.elem_size()
      if elem_sz == 0 {
        abort("Zero size type")
      }
      match self.imm32_from_val(len) {
        Some(len) => {
          let size = elem_sz * len
          let imm = self.i32_generate_imm12(size)
          let unaligned_ptr = match imm {
            Some(imm12) =>
              @generated.constructor_rv_addi(
                self,
                xreg(self.alloc_reg),
                -imm12._,
              )
            None => {
              let imm = @generated.constructor_rv_li(self, size.to_string())
              @generated.constructor_rv_sub(self, self.alloc_reg |> xreg, imm)
            }
          }
          let aligned_ptr = @generated.constructor_rv_andi(
            self, unaligned_ptr, -8,
          )
          for fill in fills {
            self.do_fill(fill, aligned_ptr |> xreg_to, elem_sz, Some(len), None)
          }
          aligned_ptr |> xreg_to
        }
        None => {
          let len = @generated.constructor_lower_value(self, len)
          let size = @generated.constructor_rv_slli(
            self,
            xreg(len),
            match elem_sz {
              4 => 2
              8 => 3
              _ => abort("Invalid type size")
            },
          )
          let unaligned_ptr = @generated.constructor_rv_sub(
            self,
            xreg(self.alloc_reg),
            size,
          )
          let aligned_ptr = @generated.constructor_rv_andi(
            self, unaligned_ptr, -8,
          )
          for fill in fills {
            self.do_fill(fill, aligned_ptr |> xreg_to, elem_sz, None, Some(len))
          }
          aligned_ptr |> xreg_to
        }
      }
    }
  }
}

fn RiscvFnLoweringCtx::lower_alloc(
  self : RiscvFnLoweringCtx,
  _ty : @core.Type,
  mref : @core.MemRef,
  fills : @core.Fills
) -> @riscv.AnyReg {
  let ptr = self.do_alloc_and_fill(mref, fills)
  let pre_assigned = self.value_regs[@core.Value::Mem(fref=self.func.id, ~mref)].unwrap()
  let _ = self.mark_maybe_aliased(ptr)
  self.alias_reg(pre_assigned, ptr)
  self.zero_reg() |> xreg_to
}

fn RiscvFnLoweringCtx::imm32_from_val(
  self : RiscvFnLoweringCtx,
  v : @core.Value
) -> Int? {
  let _ = self
  match v {
    Unit => Some(0)
    Bool(val=false) => Some(0)
    Bool(val=true) => Some(1)
    Int32(~val) => Some(val)
    Int64(~val) =>
      if val >= -2147483648 && val <= 2147483647 {
        Some(val.to_int())
      } else {
        None
      }
    _ => None
  }
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

fn ty_to_reg_class_opt(ty : @core.Type) -> RegClass? {
  match ty {
    @core.Type::Double => Some(F)
    @core.Type::Unit => None
    _ => Some(X)
  }
}

fn RiscvFnLoweringCtx::assign_reg(
  self : RiscvFnLoweringCtx,
  val : @core.Value,
  reg : @riscv.AnyReg
) -> Unit {
  self.value_regs.set(val, reg)
  self.value_ref_count.set(val, 0)
}

fn RiscvFnLoweringCtx::assign_regs_for_mem(self : RiscvFnLoweringCtx) -> Unit {
  for mem in self.func.mems {
    let (mref, _mem) = mem
    let mval = @core.Value::Mem(fref=self.func.id, ~mref)
    let reg = self.next_reg(X)
    self.assign_reg(mval, reg)
  }
}

fn RiscvFnLoweringCtx::assign_reg_for_closure_ptr(
  self : RiscvFnLoweringCtx
) -> Unit {
  let self_val = @core.Value::Self(fref=self.func.id)
  self.assign_reg(self_val, self.closure_reg)
}

fn RiscvFnLoweringCtx::assign_regs_for_free_variables(
  self : RiscvFnLoweringCtx
) -> Unit {
  for id, fv_type in self.func.fvars {
    let id = id + 1 // 0 is reserved for the closure pointer
    let reg = self.next_reg(ty_to_reg_class(fv_type))
    let val = @core.Value::FreeVar(fvref={ id, })
    self.assign_reg(val, reg)
  }
}

fn RiscvFnLoweringCtx::alias_reg(
  self : RiscvFnLoweringCtx,
  pre_assigned_reg : @riscv.AnyReg,
  actual_reg : @riscv.AnyReg
) -> Unit {
  for reg in self.ialias {
    if reg == actual_reg {
      reg.reg = pre_assigned_reg.reg
    }
  }
  self.ialias.clear()
}

fn RiscvFnLoweringCtx::finalize_insn_with_reg_aliasing(
  self : RiscvFnLoweringCtx,
  pre_assigned_reg : @riscv.AnyReg,
  actual_reg : @riscv.AnyReg
) -> Unit {
  self.alias_reg(pre_assigned_reg, actual_reg)
  self.finalize_insn()
}

fn RiscvFnLoweringCtx::finalize_insn(self : RiscvFnLoweringCtx) -> Unit {
  self.ibuffer.rev_inplace()
  self.buffer.append(self.ibuffer)
  self.ibuffer.clear()
  if self.ialias.length() > 0 {
    abort("Alias set is not empty")
  }
}

fn RiscvFnLoweringCtx::finalize_basic_block(
  self : RiscvFnLoweringCtx
) -> Array[MInsn] {
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
      let val = @core.Value::BlockParam(bref=bb.id, bpref={ id, })
      self.assign_reg(val, reg)
    }
    for iref in bb.seq {
      let ty = self.ref_insn(iref).ty()
      let reg = self.next_reg(ty_to_reg_class(ty))
      let val = @core.Value::Insn(~iref)
      self.assign_reg(val, reg)
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
    let val = @core.Value::FnParam(fref=self.func.id, fpref={ id, })
    self.assign_reg(val, reg)
  }

  match self.func.ty {
    @core.Type::ClosureFn(~params, ..) | @core.Type::DirectFn(~params, ..) =>
      for i, param in params {
        assign_with_ty(i, param)
      }
    _ => abort("Invalid function type")
  }
}

fn RiscvFnLoweringCtx::block_label(
  self : RiscvFnLoweringCtx,
  bb : @core.BlockRef
) -> @riscv.Label {
  let _ = self
  @riscv.Label::Label(".\{self.func.name}_b\{bb.id}")
}

fn RiscvFnLoweringCtx::emit_block_parameters(
  self : RiscvFnLoweringCtx,
  target : @core.BlockRef,
  args : Array[@core.Value]
) -> Unit {
  let target = self.ref_block(target).basic_block(self.func)
  for id, arg in args {
    let ty = target.params[id]
    match ty_to_reg_class(ty) {
      X => {
        let reg = @generated.constructor_lower_x_value(self, arg)
        let param = @core.Value::BlockParam(bref=target.id, bpref={ id, })
        let param_reg = self.value_regs[param].unwrap()
        @generated.constructor_rv_mv_to(self, xreg(param_reg), reg)
      }
      F => {
        let reg = @generated.constructor_lower_f_value(self, arg)
        let param = @core.Value::BlockParam(bref=target.id, bpref={ id, })
        let param_reg = self.value_regs[param].unwrap()
        @generated.constructor_rv_f_mv_to(self, freg(param_reg), reg)
      }
    }
  }
}

fn RiscvFnLoweringCtx::value_needed(
  self : RiscvFnLoweringCtx,
  v : @core.Value
) -> Bool {
  self.value_ref_count[v].unwrap() > 0
}

fn RiscvFnLoweringCtx::has_type(
  self : RiscvFnLoweringCtx,
  v : @core.Value
) -> (@core.Value, @core.Type) {
  let ty = self.type_of(v)
  (v, ty)
}

fn RiscvFnLoweringCtx::type_of(
  self : RiscvFnLoweringCtx,
  v : @core.Value
) -> @core.Type {
  let ty = try {
    v.ty!(self.core, self.func)
  } catch {
    Failure(msg) => abort(msg)
  }
  ty
}

fn RiscvFnLoweringCtx::effectful(
  self : RiscvFnLoweringCtx,
  insn : @core.Insn
) -> Bool {
  let _ = self
  match insn {
    @core.Insn::Call(..) => true
    @core.Insn::ExtCall(..) => true
    @core.Insn::Apply(..) => true
    @core.Insn::Store(..) => true
    @core.Insn::Alloc(..) => true
    _ => false
  }
}

fn RiscvFnLoweringCtx::has_result(
  self : RiscvFnLoweringCtx,
  insn : @core.Insn
) -> Bool {
  let _ = self
  match insn {
    @core.Insn::Store(..) => false
    @core.Insn::Alloc(..) => false
    _ => true
  }
}

fn RiscvFnLoweringCtx::mark_maybe_aliased(
  self : RiscvFnLoweringCtx,
  reg : @riscv.AnyReg
) -> @riscv.AnyReg {
  self.ialias.push(reg)
  reg
}

fn RiscvFnLoweringCtx::load_params(self : RiscvFnLoweringCtx) -> Unit {
  let mut used_xarg = 0
  let mut used_farg = 0
  let params = match self.func.ty {
    @core.Type::ClosureFn(~params, ..) | @core.Type::DirectFn(~params, ..) =>
      params
    _ => abort("Invalid function type")
  }
  for i, ty in params {
    let val = @core.Value::FnParam(fref=self.func.id, fpref={ id: i })
    let pre_assigned_reg = self.value_regs[val].unwrap()
    match ty_to_reg_class(ty) {
      X => {
        @generated.constructor_rv_mv_to(
          self,
          xreg(pre_assigned_reg),
          @riscv.reg_arg_list[used_xarg],
        )
        used_xarg += 1
      }
      F => {
        @generated.constructor_rv_f_mv_to(
          self,
          freg(pre_assigned_reg),
          @riscv.freg_arg_list[used_farg],
        )
        used_farg += 1
      }
    }
  }
  self.finalize_insn()
}

fn RiscvFnLoweringCtx::load_closure_pointer(self : RiscvFnLoweringCtx) -> Unit {
  match self.func.ty {
    @core.Type::ClosureFn(..) =>
      if self.func.fvars.length() > 0 {
        @generated.constructor_rv_mv_to(
          self,
          self.closure_reg |> xreg,
          @riscv.reg_closure,
        )
        self.finalize_insn()
      }
    _ => ()
  }
}

fn RiscvFnLoweringCtx::load_freevar_regs(self : RiscvFnLoweringCtx) -> Unit {
  for i, ty in self.func.fvars {
    let index = i + 1
    let base = self.closure_reg
    let val = @core.Value::FreeVar(fvref={ id: index })
    let pre_assigned_reg = self.value_regs[val].unwrap()
    let ma : @riscv.MemAccess[@riscv.Reg, @riscv.Imm12] = {
      base: xreg(base),
      offset: index * 8, // hard encoded as closure field is always 8 bytes even if there's an i32
    }
    match ty_to_reg_class(ty) {
      X => {
        let reg = @generated.constructor_rv_lw(self, ma)
        let reg = self.mark_maybe_aliased(reg |> xreg_to)
        self.alias_reg(pre_assigned_reg, reg)
      }
      F => {
        let reg = @generated.constructor_rv_fld(self, ma)
        let reg = self.mark_maybe_aliased(reg |> freg_to)
        self.alias_reg(pre_assigned_reg, reg)
      }
    }
  }
  self.finalize_insn()
}

fn RiscvFnLoweringCtx::lower_block(
  self : RiscvFnLoweringCtx,
  block : @core.BasicBlock
) -> MBasicBlock!Failure {
  let id = self.block_label(block.id)
  @generated.constructor_lower_control(self, block.control).or_error!(
    Failure("lower control failed: \{block.control}"),
  )
  self.finalize_insn()
  let control = self.finalize_basic_block()
  for iref in block.seq.rev() {
    let insn_value = @core.Value::Insn(~iref)
    let insn = self.ref_insn(iref)
    let pre_assigned_reg = self.value_regs[insn_value].or_error!(
      Failure("No reg assigned for insn \{iref}"),
    )
    if RiscvFnLoweringCtx::value_needed(self, insn_value) ||
      self.effectful(insn) {
      let reg = @generated.constructor_lower(self, insn).or_error!(
        Failure("lower insn failed: \{insn}"),
      )
      if RiscvFnLoweringCtx::has_result(self, insn) {
        self.finalize_insn_with_reg_aliasing(pre_assigned_reg, reg)
      } else {
        self.ialias.clear()
        self.finalize_insn()
      }
    } else {
      trace("skipping \{insn}")
    }
  }
  if block.id == self.func.entry {
    self.load_freevar_regs()
    self.load_params()
    self.load_closure_pointer()
  }
  let insns = self.finalize_basic_block()
  { id, insns, control }
}

pub fn RiscvFnLoweringCtx::lower(self : RiscvFnLoweringCtx) -> MFn!Failure {
  let blocks = MutMap::new()
  for block in self.order.order.rev() {
    match block {
      BasicBlock(~bb) =>
        blocks.set(self.block_label(bb.id), self.lower_block!(bb))
      _ => fail!("Critical edge not supported")
    }
  }
  let entry = self.block_label(self.func.entry)
  { name: self.func.name, entry, blocks }
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
  let reg_classes = [X, X]
  let closure_reg = @riscv.AnyReg::{ reg: 0 }
  let alloc_reg = @riscv.reg_alloc.phy_to_virt() // s11 is reserved for the allocator
  let order = @lower.LoweringOrder::new(func)
  let lower_ctx = RiscvFnLoweringCtx::{
    core,
    func,
    reg_classes,
    value_ref_count,
    value_regs,
    closure_reg,
    alloc_reg,
    order,
    buffer: [],
    ibuffer: [],
    ialias: [],
  }
  lower_ctx.assign_reg_for_closure_ptr()
  lower_ctx.assign_regs_for_function_parameters()
  lower_ctx.assign_regs_for_free_variables()
  lower_ctx.assign_regs_for_mem()
  lower_ctx.assign_regs_for_block_params_and_instrs()
  trace("pre_assigned_registers: \{lower_ctx.value_regs_to_string()}")
  lower_ctx
}

pub struct RiscvLoweringCtx {
  core : @core.Core
}

pub fn RiscvLoweringCtx::new(core : @core.Core) -> RiscvLoweringCtx {
  { core, }
}

pub fn RiscvLoweringCtx::lower_globals(
  self : RiscvLoweringCtx,
  globals : MutMap[@riscv.Label, Int]
) -> MFn {
  let core = self.core
  let name = @riscv.Label::Label("_start")
  let entry = @riscv.Label::Label("_start_b0")
  let blocks = MutMap::new()
  let insns = []
  for symbols in core.global_symbols {
    let (_, symbol) = symbols
    let ty_size = symbol.ty.size()
    let rc = ty_to_reg_class(symbol.ty)
    let thunk = core.funcs.get(symbol.thunk).unwrap()
    globals.set(
      @riscv.Label::Label(symbol.name.to_string()),
      if ty_size == 0 {
        4
      } else {
        ty_size
      },
    )
    insns.push(MInsn::Call(target=Label(thunk.name.to_string())))
    insns.push(MInsn::La(rd=@riscv.reg_swap, l=Label(symbol.name.to_string())))
    match (rc, ty_size) {
      (X, 4) =>
        insns.push(
          MInsn::Sw(rs=@riscv.reg_ret, mem={ base: @riscv.reg_swap, offset: 0 }),
        )
      (X, 8) =>
        insns.push(
          MInsn::Sd(rs=@riscv.reg_ret, mem={ base: @riscv.reg_swap, offset: 0 }),
        )
      (F, 8) =>
        insns.push(
          MInsn::Fsd(rs=@riscv.Fa0, mem={ base: @riscv.reg_swap, offset: 0 }),
        )
      (_, 0) => {
        let _ = insns.unsafe_pop()

      }
      _ => abort("Invalid thunk type size")
    }
  }
  let control = [MInsn::Ret]
  blocks.set(entry, { id: entry, insns, control })
  { name, entry, blocks }
}

pub fn RiscvLoweringCtx::lower(self : RiscvLoweringCtx) -> MCore!Failure {
  let funcs = MutMap::new()
  for func in self.core.funcs {
    let (_, func) = func
    let lower_ctx = RiscvFnLoweringCtx::new(self.core, func)
    let mfn = lower_ctx.lower!()
    funcs.set(@riscv.Label::Label(func.name), mfn)
  }
  let globals = MutMap::new()
  let init = self.lower_globals(globals)
  funcs.set(init.name, init)
  { funcs, globals }
}
