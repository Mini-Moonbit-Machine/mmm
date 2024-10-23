typealias MutMap[K, V] = @hashmap.T[K, V]

typealias MutSet[K] = @hashset.T[K]

typealias RegClass = @riscv.RegClass

pub struct RiscvFnLoweringCtx {
  core : @core.Core
  func : @core.Fn
  reg_classes : Array[RegClass]
  value_ref_count : MutMap[@core.Value, Int]
  value_regs : MutMap[@core.Value, AnyReg]
  closure_reg : AnyReg
  alloc_reg : AnyReg
  order : @lower.LoweringOrder
  mut buffer : Array[MInsn]
  ibuffer : Array[MInsn]
  ialias : Array[AnyReg]
  block_params_assignment : MutMap[@core.BlockRef, Array[AnyReg]]
  used_primitive : MutSet[String]
  mut is_leaf : Bool
  mut mfn : MFn?
  tracer : @util.SubTracer
}

fn RiscvFnLoweringCtx::value_regs_to_string(
  self : RiscvFnLoweringCtx
) -> String {
  fn pretty_reg(kv : (@core.Value, AnyReg)) -> String {
    "\{kv.0} -> \{kv.1}"
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
    self.tracer.abort("Invalid free var ref")
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
    _ => self.tracer.abort("Invalid type")
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
) -> AnyReg {
  let id = reg_classes.length()
  reg_classes.push(reg_class)
  AnyReg::{ reg: id, class: reg_class }
}

fn RiscvFnLoweringCtx::next_reg(
  self : RiscvFnLoweringCtx,
  reg_class : RegClass
) -> AnyReg {
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

fn xreg(r : AnyReg) -> @riscv.Reg {
  r.to_xreg()
}

fn freg(r : AnyReg) -> @riscv.FReg {
  r.to_freg()
}

fn RiscvFnLoweringCtx::xreg_new(
  self : RiscvFnLoweringCtx,
  r : AnyReg
) -> @riscv.Reg {
  let _ = self
  r |> xreg
}

fn RiscvFnLoweringCtx::freg_new(
  self : RiscvFnLoweringCtx,
  r : AnyReg
) -> @riscv.FReg {
  let _ = self
  r |> freg
}

fn xreg_to(r : @riscv.Reg) -> AnyReg {
  r.to_any()
}

fn freg_to(r : @riscv.FReg) -> AnyReg {
  r.to_any()
}

fn RiscvFnLoweringCtx::xreg_to_reg(
  self : RiscvFnLoweringCtx,
  r : @riscv.Reg
) -> AnyReg {
  let _ = self
  r |> xreg_to
}

fn RiscvFnLoweringCtx::freg_to_reg(
  self : RiscvFnLoweringCtx,
  r : @riscv.FReg
) -> AnyReg {
  let _ = self
  r |> freg_to
}

fn RiscvFnLoweringCtx::put_in_reg(
  self : RiscvFnLoweringCtx,
  v : @core.Value
) -> AnyReg {
  let r = self.value_regs[v].or_else(
    fn() { self.tracer.abort("No reg assigned for value \{v}") },
  )
  self.value_ref_count[v] = self.value_ref_count[v].or_else(
      fn() { self.tracer.abort("No ref count assigned for value \{v}") },
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

fn RiscvFnLoweringCtx::is_float_return_reg(
  self : RiscvFnLoweringCtx,
  r : @riscv.FReg
) -> Unit? {
  let _ = self
  if r == @riscv.freg_ret {
    Some(())
  } else {
    None
  }
}

fn RiscvFnLoweringCtx::float_return_reg(
  self : RiscvFnLoweringCtx
) -> @riscv.FReg {
  let _ = self
  @riscv.freg_ret
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
  @riscv.Imm12::from_int64(i64)
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
  let _ = self
  @riscv.Imm12::from_int32(i32)
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
) -> (Int, Int) {
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
          self.tracer.abort("Too many arguments")
        }
      }
      F => {
        @generated.constructor_rv_fmv_to(
          self,
          @riscv.freg_arg_list[used_farg],
          freg(r),
        )
        used_farg += 1
        if used_farg >= @riscv.freg_arg_list.length() {
          self.tracer.abort("Too many arguments")
        }
      }
    }
  }
  (used_xarg, used_farg)
}

fn RiscvFnLoweringCtx::copy_return_value(
  self : RiscvFnLoweringCtx,
  ty : @core.Type
) -> AnyReg {
  match ty_to_reg_class_opt(ty) {
    Some(X) => xreg_to(@generated.constructor_rv_mv(self, @riscv.reg_ret))
    Some(F) => freg_to(@generated.constructor_rv_fmv_d(self, @riscv.freg_ret))
    None => xreg_to(@generated.constructor_rv_mv(self, @riscv.reg_ret))
  }
}

fn RiscvFnLoweringCtx::lower_call(
  self : RiscvFnLoweringCtx,
  ty : @core.Type,
  f : @core.Fn,
  args : Array[@core.Value]
) -> AnyReg {
  self.is_leaf = false
  @generated.constructor_rv_save_ctx2(self)
  let (nx, nf) = self.copy_call_arguments(args)
  @generated.constructor_rv_call(self, Label(f.name.to_string()), nx, nf)
  let r = self.copy_return_value(ty)
  @generated.constructor_rv_restore_ctx2(self)
  r
}

fn RiscvFnLoweringCtx::lower_ext_call(
  self : RiscvFnLoweringCtx,
  ty : @core.Type,
  f : String,
  args : Array[@core.Value]
) -> AnyReg {
  self.is_leaf = false
  @generated.constructor_rv_save_ctx2(self)
  let (nx, nf) = self.copy_call_arguments(args)
  let map = {}
  @generated.constructor_rv_call(self, Label(map.get(f).or(f)), nx, nf)
  let r = self.copy_return_value(ty)
  @generated.constructor_rv_restore_ctx2(self)
  r
}

fn RiscvFnLoweringCtx::lower_apply(
  self : RiscvFnLoweringCtx,
  ty : @core.Type,
  f : @core.Value,
  args : Array[@core.Value]
) -> AnyReg {
  self.is_leaf = false
  let closure_addr = @generated.constructor_lower_value(self, f)
  @generated.constructor_rv_mv_to(self, @riscv.reg_closure, xreg(closure_addr))
  let func_ma : @riscv.MemAccess[@riscv.Reg, @riscv.Imm12] = {
    base: xreg(closure_addr),
    offset: 0,
  }
  let fp = @generated.constructor_rv_ld(self, func_ma)
  @generated.constructor_rv_save_ctx2(self)
  let (nx, nf) = self.copy_call_arguments(args)
  @generated.constructor_rv_jalr(self, fp, nx, nf)
  let r = self.copy_return_value(ty)
  @generated.constructor_rv_restore_ctx2(self)
  r
}

fn RiscvFnLoweringCtx::lower_load_fn(
  self : RiscvFnLoweringCtx,
  func : @core.FnRef
) -> AnyReg {
  @generated.constructor_rv_la(self, self.ref_fn(func).name) |> xreg_to
}

fn RiscvFnLoweringCtx::lower_load_symbol(
  self : RiscvFnLoweringCtx,
  ty : @core.Type,
  sym : @core.SymbolRef
) -> AnyReg {
  match ty_to_reg_class_opt(ty) {
    Some(c) =>
      match (ty.size(), c) {
        (4, X) =>
          xreg_to(
            @generated.constructor_rv_lwsym(self, self.ref_symbol(sym).name),
          )
        (8, X) =>
          xreg_to(
            @generated.constructor_rv_ldsym(self, self.ref_symbol(sym).name),
          )
        (8, F) => {
          let treg = @riscv.reg_swap[0]
          freg_to(
            @generated.constructor_rv_fldsym(
              self,
              self.ref_symbol(sym).name,
              treg,
            ),
          )
        }
        _ => self.tracer.abort("Invalid type size")
      }
    None => xreg_to(@generated.constructor_rv_mv(self, self.zero_reg()))
  }
}

fn RiscvFnLoweringCtx::lower_address(
  self : RiscvFnLoweringCtx,
  addr : @core.Address
) -> @riscv.MemAccess[@riscv.Reg, @riscv.Imm12] {
  match addr {
    Field(~val, ~index, ~size, ..) => {
      let base = @generated.constructor_lower_value(self, val)
      let offset = index * size
      let imm = self
        .i32_generate_imm(offset)
        .or_else(fn() { self.tracer.abort("offset too large") })
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
    Offset(~val, ~offset, ~size, ..) => {
      let base = @generated.constructor_lower_value(self, val)
      let index = @generated.constructor_lower_value(self, offset)
      let offset = match size {
        4 => @generated.constructor_rv_slliw(self, xreg(index), 2)
        8 => @generated.constructor_rv_slliw(self, xreg(index), 3)
        _ => self.tracer.abort("Invalid type size")
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
) -> AnyReg {
  let r = @generated.constructor_lower_value(self, val)
  match ty_to_reg_class_opt(ty) {
    Some(c) => {
      let ma = self.lower_address(addr)
      match (ty.size(), c) {
        (4, X) => @generated.constructor_rv_sw(self, xreg(r), ma)
        (8, X) => @generated.constructor_rv_sd(self, xreg(r), ma)
        (8, F) => @generated.constructor_rv_fsd(self, freg(r), ma)
        _ => self.tracer.abort("Invalid type size")
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
) -> AnyReg {
  match ty_to_reg_class_opt(ty) {
    Some(c) => {
      let ma = self.lower_address(addr)
      match (ty.size(), c) {
        (4, X) => xreg_to(@generated.constructor_rv_lw(self, ma))
        (8, X) => xreg_to(@generated.constructor_rv_ld(self, ma))
        (8, F) => freg_to(@generated.constructor_rv_fld(self, ma))
        _ => self.tracer.abort("Invalid type size")
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
  ptr : AnyReg,
  elem_sz : Int,
  init_rc : RegClass,
  init_reg : AnyReg
) -> Unit {
  self.is_leaf = false
  match fill {
    Init(..) =>
      match (elem_sz, init_rc) {
        (4, X) => {
          @generated.constructor_rv_mv_to(self, @riscv.reg_ret, xreg(ptr))
          // A1 is assumed to be the length of the buffer
          @generated.constructor_rv_mv_to(self, @riscv.A2, xreg(init_reg))
          @generated.constructor_rv_call(self, Label("memseti32"), 3, 0)
          self.used_primitive.insert("memseti32")
        }
        (8, X) => {
          @generated.constructor_rv_mv_to(self, @riscv.reg_ret, xreg(ptr))
          // A1 is assumed to be the length of the buffer
          @generated.constructor_rv_mv_to(self, @riscv.A2, xreg(init_reg))
          @generated.constructor_rv_call(self, Label("memseti64"), 3, 0)
          self.used_primitive.insert("memseti64")
        }
        (8, F) => {
          @generated.constructor_rv_mv_to(self, @riscv.reg_ret, xreg(ptr))
          // A1 is assumed to be the length of the buffer
          @generated.constructor_rv_fmv_to(self, @riscv.Fa0, freg(init_reg))
          @generated.constructor_rv_call(self, Label("memsetf64"), 2, 1)
          self.used_primitive.insert("memsetf64")
        }
        _ => self.tracer.abort("Invalid fill")
      }
    Zero => {
      @generated.constructor_rv_mv_to(self, @riscv.reg_ret, xreg(ptr))
      @generated.constructor_rv_call(self, Label("memset0e\{elem_sz}"), 2, 0)
      self.used_primitive.insert("memset0e\{elem_sz}")
    }
    _ => self.tracer.abort("Invalid fill")
  }
}

fn RiscvFnLoweringCtx::do_fill_at_small_offset(
  self : RiscvFnLoweringCtx,
  ptr : AnyReg,
  elem_sz : Int,
  index : @riscv.Imm12,
  init_rc : RegClass,
  init_reg : AnyReg
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
    _ => self.tracer.abort("Invalid type size")
  }
}

fn RiscvFnLoweringCtx::get_init_reg_with_class(
  self : RiscvFnLoweringCtx,
  fill : @core.Fill
) -> (RegClass, AnyReg) {
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
  ptr : AnyReg,
  elem_sz : Int,
  index : Int
) -> Unit {
  let (init_rc, init_reg) = self.get_init_reg_with_class(fill)
  let imm_offset = self.i32_generate_imm12(index * elem_sz)
  match imm_offset {
    Some(imm12) =>
      self.do_fill_at_small_offset(ptr, elem_sz, imm12, init_rc, init_reg)
    None => self.tracer.abort("Field offset too large")
  }
}

fn RiscvFnLoweringCtx::do_fill(
  self : RiscvFnLoweringCtx,
  fill : @core.Fill,
  ptr : AnyReg,
  elem_sz : Int,
  imm_len : Int?,
  reg_len : AnyReg?
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
        None => self.tracer.abort("Invalid length")
      }
  }
}

fn RiscvFnLoweringCtx::do_alloc_and_fill(
  self : RiscvFnLoweringCtx,
  mref : @core.MemRef,
  fills : @core.Fills
) -> AnyReg {
  let mem = self.ref_mem(mref)
  match mem {
    Stack(..) => self.tracer.abort("Stack allocation not supported")
    Heap(~len, ~ty) => {
      let elem_sz = ty.elem_size()
      if elem_sz == 0 {
        self.tracer.abort("Zero size type")
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
          @generated.constructor_rv_mv_to(
            self,
            self.alloc_reg |> xreg,
            aligned_ptr,
          )
          for fill in fills {
            self.do_fill(fill, aligned_ptr |> xreg_to, elem_sz, Some(len), None)
          }
          aligned_ptr |> xreg_to
        }
        None => {
          let len = @generated.constructor_lower_value(self, len)
          let size = @generated.constructor_rv_slliw(
            self,
            xreg(len),
            match elem_sz {
              4 => 2
              8 => 3
              _ => self.tracer.abort("Invalid type size")
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
          @generated.constructor_rv_mv_to(
            self,
            self.alloc_reg |> xreg,
            aligned_ptr,
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
) -> AnyReg {
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

fn RiscvFnLoweringCtx::imm12_from_neg_val(
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
    Some((imm20, imm12)) =>
      if imm20._ == 0 && imm12._ != -2048 {
        Some(-imm12._)
      } else {
        None
      }
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
  reg : AnyReg
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
  pre_assigned_reg : AnyReg,
  actual_reg : AnyReg
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
  pre_assigned_reg : AnyReg,
  actual_reg : AnyReg
) -> Unit {
  self.alias_reg(pre_assigned_reg, actual_reg)
  self.finalize_insn()
}

fn RiscvFnLoweringCtx::finalize_insn(self : RiscvFnLoweringCtx) -> Unit {
  self.ibuffer.rev_inplace()
  self.buffer.append(self.ibuffer)
  self.ibuffer.clear()
  if self.ialias.length() > 0 {
    self.tracer.abort("Alias set is not empty")
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
    let assignment = []
    for id, param in bb.params {
      let reg = self.next_reg(ty_to_reg_class(param))
      let val = @core.Value::BlockParam(bref=bb.id, bpref={ id, })
      self.assign_reg(val, reg)
      assignment.push(reg)
    }
    self.block_params_assignment.set(bb.id, assignment)
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
    _ => self.tracer.abort("Invalid function type")
  }
}

pub fn fn_block_name(func : @core.Fn, bb : @core.BlockRef) -> String {
  ".\{func.name}_\{bb.id}"
}

fn RiscvFnLoweringCtx::block_label(
  self : RiscvFnLoweringCtx,
  bb : @core.BlockRef
) -> @riscv.Label {
  let _ = self
  Label(fn_block_name(self.func, bb))
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
        @generated.constructor_rv_fmv_to(self, freg(param_reg), reg)
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

fn RiscvFnLoweringCtx::not_double(
  self : RiscvFnLoweringCtx,
  v : @core.Value
) -> @core.Value? {
  let ty = self.type_of(v)
  match ty {
    @core.Type::Double => None
    _ => Some(v)
  }
}

fn RiscvFnLoweringCtx::type_of(
  self : RiscvFnLoweringCtx,
  v : @core.Value
) -> @core.Type {
  let ty = try {
    v.ty!(self.core, self.func)
  } catch {
    Failure(msg) => self.tracer.abort(msg)
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
  reg : AnyReg
) -> AnyReg {
  self.ialias.push(reg)
  reg
}

fn RiscvFnLoweringCtx::load_params(self : RiscvFnLoweringCtx) -> Unit {
  let mut used_xarg = 0
  let mut used_farg = 0
  let params = match self.func.ty {
    @core.Type::ClosureFn(~params, ..) | @core.Type::DirectFn(~params, ..) =>
      params
    _ => self.tracer.abort("Invalid function type")
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
        @generated.constructor_rv_fmv_to(
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
    @core.Type::ClosureFn(..) => {
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

fn RiscvFnLoweringCtx::emit_fake_prologue(self : RiscvFnLoweringCtx) -> Unit {
  @generated.constructor_rv_save_ctx1(self)
  self.finalize_insn()
}

fn RiscvFnLoweringCtx::emit_fake_epilogue(self : RiscvFnLoweringCtx) -> Unit {
  @generated.constructor_rv_restore_ctx1(self)
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
    let size = ty.size()
    match (ty_to_reg_class(ty), size) {
      (X, 0) =>
        @generated.constructor_rv_mv_to(
          self,
          pre_assigned_reg.to_xreg(),
          self.zero_reg(),
        )
      (X, 4) => {
        let reg = @generated.constructor_rv_lw(self, ma)
        let reg = self.mark_maybe_aliased(reg |> xreg_to)
        self.alias_reg(pre_assigned_reg, reg)
      }
      (X, 8) => {
        let reg = @generated.constructor_rv_ld(self, ma)
        let reg = self.mark_maybe_aliased(reg |> xreg_to)
        self.alias_reg(pre_assigned_reg, reg)
      }
      (F, 8) => {
        let reg = @generated.constructor_rv_fld(self, ma)
        let reg = self.mark_maybe_aliased(reg |> freg_to)
        self.alias_reg(pre_assigned_reg, reg)
      }
      _ => self.tracer.abort("invalid freevar type or size")
    }
  }
  self.finalize_insn()
}

fn RiscvFnLoweringCtx::target_color(
  self : RiscvFnLoweringCtx,
  target : @core.BlockRef
) -> (@lower.ColorSeq, @core.BlockRef) {
  let color = self.order.color[target].unwrap()
  let n = self.order.color_seq[target].unwrap()
  ((color, n), target)
}

fn RiscvFnLoweringCtx::lower_block(
  self : RiscvFnLoweringCtx,
  block : @core.BasicBlock,
  color : Int,
  n : Int
) -> MBasicBlock!Failure {
  let id = self.block_label(block.id)
  @generated.constructor_lower_control(self, (color, n), block.control).or_error!(
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
      self.tracer.log("skipping \{insn}")
    }
  }
  if block.id == self.func.entry {
    self.load_freevar_regs()
    self.load_params()
    self.load_closure_pointer()
    self.emit_fake_prologue()
  }
  let insns = self.finalize_basic_block()
  let params = self.block_params_assignment[block.id].unwrap()
  { id, insns, control, params }
}

fn RiscvFnLoweringCtx::lower(
  self : RiscvFnLoweringCtx
) -> (MFn, Liveness)!Failure {
  let blocks = MutMap::new()
  for block in self.order.order.rev() {
    match block {
      BasicBlock(~bb) => {
        let color = self.order.color[bb.id].unwrap()
        let seq_in_color = self.order.color_seq[bb.id].unwrap()
        blocks.set(
          self.block_label(bb.id),
          self.lower_block!(bb, color, seq_in_color),
        )
      }
      _ => fail!("Critical edge not supported")
    }
  }
  let entry = self.block_label(self.func.entry)
  let seq = self.order.seq.map(fn(bref) { self.block_label(bref) })
  let mfn = { name: self.func.name, entry, blocks, leaf: self.is_leaf, seq }
  let liveness = Liveness::new(Some(self.func), mfn)
  liveness.compute_block_liveness()
  self.mfn = Some(mfn)
  (mfn, liveness)
}

fn subst_vreg_with_preg_or_spill(
  self : RiscvFnLoweringCtx,
  alloc : RegAllocator,
  frame : FrameLayout,
  insn : MInsn,
  buf : Array[MInsn]
) -> Unit {
  fn f(reg : AnyReg) -> AnyReg? {
    alloc.query_assigned_color(reg)
  }

  let input_xreg_spilled = []
  fn fx(reg : @riscv.Reg) -> @riscv.Reg {
    match f(reg.to_any()) {
      Some(r) => r.to_xreg()
      None =>
        if reg.to_any().is_phy_xreg() {
          reg
        } else if input_xreg_spilled.length() < 2 {
          input_xreg_spilled.push(reg)
          @riscv.reg_swap[input_xreg_spilled.length() - 1]
        } else {
          self.tracer.abort(
            "Two input xregs are spilled \{Show::to_string(insn)}",
          )
        }
    }
  }

  let input_freg_spilled = []
  fn ff(reg : @riscv.FReg) -> @riscv.FReg {
    match f(reg.to_any()) {
      Some(r) => r.to_freg()
      None =>
        if reg.to_any().is_phy_freg() {
          reg
        } else if input_freg_spilled.length() < 2 {
          input_freg_spilled.push(reg)
          @riscv.freg_swap[input_freg_spilled.length() - 1]
        } else {
          self.tracer.abort(
            "Two input fregs are spilled \{Show::to_string(insn)}",
          )
        }
    }
  }

  fn fmem(
    mem : @riscv.MemAccess[@riscv.Reg, @riscv.Imm12]
  ) -> @riscv.MemAccess[@riscv.Reg, @riscv.Imm12] {
    let base = fx(mem.base)
    { base, offset: mem.offset }
  }

  let output_xreg_spilled = []
  fn frx(reg : @riscv.Reg) -> @riscv.Reg {
    match f(reg.to_any()) {
      Some(r) => r.to_xreg()
      None =>
        if reg.to_any().is_phy_xreg() {
          reg
        } else if output_xreg_spilled.length() < 2 {
          output_xreg_spilled.push(reg)
          @riscv.reg_swap[output_xreg_spilled.length() - 1]
        } else {
          self.tracer.abort(
            "Two output xregs are spilled \{Show::to_string(insn)}",
          )
        }
    }
  }

  let output_freg_spilled = []
  fn frf(reg : @riscv.FReg) -> @riscv.FReg {
    match f(reg.to_any()) {
      Some(r) => r.to_freg()
      None =>
        if reg.to_any().is_phy_freg() {
          reg
        } else if output_freg_spilled.length() < 2 {
          output_freg_spilled.push(reg)
          @riscv.freg_swap[output_freg_spilled.length() - 1]
        } else {
          self.tracer.abort(
            "Two output fregs are spilled \{Show::to_string(insn)}",
          )
        }
    }
  }

  let insn2 = match insn {
    Add(~rd, ~rs1, ~rs2) => MInsn::Add(rd=frx(rd), rs1=fx(rs1), rs2=fx(rs2))
    Sub(~rd, ~rs1, ~rs2) => Sub(rd=frx(rd), rs1=fx(rs1), rs2=fx(rs2))
    Xor(~rd, ~rs1, ~rs2) => Xor(rd=frx(rd), rs1=fx(rs1), rs2=fx(rs2))
    Or(~rd, ~rs1, ~rs2) => Or(rd=frx(rd), rs1=fx(rs1), rs2=fx(rs2))
    And(~rd, ~rs1, ~rs2) => And(rd=frx(rd), rs1=fx(rs1), rs2=fx(rs2))
    Sll(~rd, ~rs1, ~rs2) => Sll(rd=frx(rd), rs1=fx(rs1), rs2=fx(rs2))
    Srl(~rd, ~rs1, ~rs2) => Srl(rd=frx(rd), rs1=fx(rs1), rs2=fx(rs2))
    Sra(~rd, ~rs1, ~rs2) => Sra(rd=frx(rd), rs1=fx(rs1), rs2=fx(rs2))
    Slt(~rd, ~rs1, ~rs2) => Slt(rd=frx(rd), rs1=fx(rs1), rs2=fx(rs2))
    Sltu(~rd, ~rs1, ~rs2) => Sltu(rd=frx(rd), rs1=fx(rs1), rs2=fx(rs2))
    Addi(~rd, ~rs1, ~imm) => Addi(rd=frx(rd), rs1=fx(rs1), ~imm)
    Xori(~rd, ~rs1, ~imm) => Xori(rd=frx(rd), rs1=fx(rs1), ~imm)
    Ori(~rd, ~rs1, ~imm) => Ori(rd=frx(rd), rs1=fx(rs1), ~imm)
    Andi(~rd, ~rs1, ~imm) => Andi(rd=frx(rd), rs1=fx(rs1), ~imm)
    Slli(~rd, ~rs1, ~imm) => Slli(rd=frx(rd), rs1=fx(rs1), ~imm)
    Slliw(~rd, ~rs1, ~imm) => Slliw(rd=frx(rd), rs1=fx(rs1), ~imm)
    Srli(~rd, ~rs1, ~imm) => Srli(rd=frx(rd), rs1=fx(rs1), ~imm)
    Srai(~rd, ~rs1, ~imm) => Srai(rd=frx(rd), rs1=fx(rs1), ~imm)
    Slti(~rd, ~rs1, ~imm) => Slti(rd=frx(rd), rs1=fx(rs1), ~imm)
    Sltiu(~rd, ~rs1, ~imm) => Sltiu(rd=frx(rd), rs1=fx(rs1), ~imm)
    Lbu(~rd, ~mem) => Lbu(rd=frx(rd), mem=fmem(mem))
    Lhu(~rd, ~mem) => Lhu(rd=frx(rd), mem=fmem(mem))
    Lwu(~rd, ~mem) => Lwu(rd=frx(rd), mem=fmem(mem))
    Lb(~rd, ~mem) => Lb(rd=frx(rd), mem=fmem(mem))
    Lh(~rd, ~mem) => Lh(rd=frx(rd), mem=fmem(mem))
    Lw(~rd, ~mem) => Lw(rd=frx(rd), mem=fmem(mem))
    Ld(~rd, ~mem) => Ld(rd=frx(rd), mem=fmem(mem))
    Sb(~rs, ~mem) => Sb(rs=fx(rs), mem=fmem(mem))
    Sh(~rs, ~mem) => Sh(rs=fx(rs), mem=fmem(mem))
    Sw(~rs, ~mem) => Sw(rs=fx(rs), mem=fmem(mem))
    Sd(~rs, ~mem) => Sd(rs=fx(rs), mem=fmem(mem))
    Beq(~rs1, ~rs2, ~target) => Beq(rs1=fx(rs1), rs2=fx(rs2), ~target)
    Bne(~rs1, ~rs2, ~target) => Bne(rs1=fx(rs1), rs2=fx(rs2), ~target)
    Blt(~rs1, ~rs2, ~target) => Blt(rs1=fx(rs1), rs2=fx(rs2), ~target)
    Bge(~rs1, ~rs2, ~target) => Bge(rs1=fx(rs1), rs2=fx(rs2), ~target)
    Ble(~rs1, ~rs2, ~target) => Ble(rs1=fx(rs1), rs2=fx(rs2), ~target)
    Bgt(~rs1, ~rs2, ~target) => Bgt(rs1=fx(rs1), rs2=fx(rs2), ~target)
    Bltu(~rs1, ~rs2, ~target) => Bltu(rs1=fx(rs1), rs2=fx(rs2), ~target)
    Bgeu(~rs1, ~rs2, ~target) => Bgeu(rs1=fx(rs1), rs2=fx(rs2), ~target)
    Ecall => Ecall
    Mul(~rd, ~rs1, ~rs2) => Mul(rd=frx(rd), rs1=fx(rs1), rs2=fx(rs2))
    Mulw(~rd, ~rs1, ~rs2) => Mulw(rd=frx(rd), rs1=fx(rs1), rs2=fx(rs2))
    Mulh(~rd, ~rs1, ~rs2) => Mulh(rd=frx(rd), rs1=fx(rs1), rs2=fx(rs2))
    Mulhsu(~rd, ~rs1, ~rs2) => Mulhsu(rd=frx(rd), rs1=fx(rs1), rs2=fx(rs2))
    Mulhu(~rd, ~rs1, ~rs2) => Mulhu(rd=frx(rd), rs1=fx(rs1), rs2=fx(rs2))
    Div(~rd, ~rs1, ~rs2) => Div(rd=frx(rd), rs1=fx(rs1), rs2=fx(rs2))
    Divw(~rd, ~rs1, ~rs2) => Divw(rd=frx(rd), rs1=fx(rs1), rs2=fx(rs2))
    Divu(~rd, ~rs1, ~rs2) => Divu(rd=frx(rd), rs1=fx(rs1), rs2=fx(rs2))
    Rem(~rd, ~rs1, ~rs2) => Rem(rd=frx(rd), rs1=fx(rs1), rs2=fx(rs2))
    Remw(~rd, ~rs1, ~rs2) => Remw(rd=frx(rd), rs1=fx(rs1), rs2=fx(rs2))
    Remu(~rd, ~rs1, ~rs2) => Remu(rd=frx(rd), rs1=fx(rs1), rs2=fx(rs2))
    FaddD(~rd, ~rs1, ~rs2) => FaddD(rd=frf(rd), rs1=ff(rs1), rs2=ff(rs2))
    FsubD(~rd, ~rs1, ~rs2) => FsubD(rd=frf(rd), rs1=ff(rs1), rs2=ff(rs2))
    FmulD(~rd, ~rs1, ~rs2) => FmulD(rd=frf(rd), rs1=ff(rs1), rs2=ff(rs2))
    FdivD(~rd, ~rs1, ~rs2) => FdivD(rd=frf(rd), rs1=ff(rs1), rs2=ff(rs2))
    Fld(~rd, ~mem) => Fld(rd=frf(rd), mem=fmem(mem))
    Fsd(~rs, ~mem) => Fsd(rs=ff(rs), mem=fmem(mem))
    FeqD(~rd, ~rs1, ~rs2) => FeqD(rd=frx(rd), rs1=ff(rs1), rs2=ff(rs2))
    FleD(~rd, ~rs1, ~rs2) => FleD(rd=frx(rd), rs1=ff(rs1), rs2=ff(rs2))
    FmvDX(~rd, ~rs) => FmvDX(rd=frf(rd), rs=fx(rs))
    FcvtWD(~rd, ~rs, ~rm) => FcvtWD(rd=frx(rd), rs=ff(rs), ~rm)
    FcvtDW(~rd, ~rs) => FcvtDW(rd=frf(rd), rs=fx(rs))
    FabsD(~rd, ~rs) => FabsD(rd=frf(rd), rs=ff(rs))
    FsqrtD(~rd, ~rs) => FsqrtD(rd=frf(rd), rs=ff(rs))
    FmaddD(~rd, ~rs1, ~rs2, ~rs3) =>
      FmaddD(rd=frf(rd), rs1=ff(rs1), rs2=ff(rs2), rs3=ff(rs3))
    FmsubD(~rd, ~rs1, ~rs2, ~rs3) =>
      FmsubD(rd=frf(rd), rs1=ff(rs1), rs2=ff(rs2), rs3=ff(rs3))
    FnmsubD(~rd, ~rs1, ~rs2, ~rs3) =>
      FnmsubD(rd=frf(rd), rs1=ff(rs1), rs2=ff(rs2), rs3=ff(rs3))
    LdSym(~rd, ~l) => LdSym(rd=frx(rd), ~l)
    LwSym(~rd, ~l) => LwSym(rd=frx(rd), ~l)
    // TODO: rt need to be handled specially
    FldSym(~rd, ~l, ~rt) => FldSym(rd=frf(rd), ~l, rt=fx(rt))
    La(~rd, ~l) => La(rd=frx(rd), ~l)
    Li(~rd, ~i) => Li(rd=frx(rd), ~i)
    Neg(~rd, ~rs) => Neg(rd=frx(rd), rs=fx(rs))
    Mv(~rd, ~rs) => {
      let rd = frx(rd)
      let rs = fx(rs)
      if rd == rs {
        return
      }
      Mv(~rd, ~rs)
    }
    FnegD(~rd, ~rs) => FnegD(rd=frf(rd), rs=ff(rs))
    FmvD(~rd, ~rs) => {
      let rd = frf(rd)
      let rs = ff(rs)
      if rd == rs {
        return
      }
      FmvD(~rd, ~rs)
    }
    XRet => XRet
    FRet => FRet
    Call(~num_xregs, ~num_fregs, ~target) =>
      Call(~num_xregs, ~num_fregs, ~target)
    Jalr(~num_xregs, ~num_fregs, ~target) =>
      Jalr(~num_xregs, ~num_fregs, target=fx(target))
    SaveCtx1 => SaveCtx1
    RestoreCtx1 => RestoreCtx1
    SaveCtx2 => SaveCtx2
    RestoreCtx2 => RestoreCtx2
    Jr(~target) => Jr(target=fx(target))
    J(~target) => J(~target)
    Nop => Nop
    Tail(~target) => Tail(~target)
    Label(x) => Label(x)
    Comment(x) => Comment(x)
  }
  for i, x in input_xreg_spilled {
    let slot = alloc
      .query_spill(x.to_any())
      .or_else(fn() { self.tracer.abort("spill \{x.to_any()} not found") })
    let offset = frame.spill[slot].or_else(
      fn() { self.tracer.abort("spill offset \{x.to_any()} not found") },
    )
    buf.push(MInsn::Lw(rd=@riscv.reg_swap[i], mem={ base: @riscv.Sp, offset }))
  }
  for i, f in input_freg_spilled {
    let slot = alloc
      .query_spill(f.to_any())
      .or_else(fn() { self.tracer.abort("spill \{f.to_any()} not found") })
    let offset = frame.spill[slot].or_else(
      fn() { self.tracer.abort("spill offset \{f.to_any()} not found") },
    )
    buf.push(
      MInsn::Fld(rd=@riscv.freg_swap[i], mem={ base: @riscv.Sp, offset }),
    )
  }
  self.tracer.println(
    "subst \{Show::to_string(insn)} -> \{Show::to_string(insn2)}",
  )
  buf.push(insn2)
  for i, x in output_xreg_spilled {
    let slot = alloc
      .query_spill(x.to_any())
      .or_else(fn() { self.tracer.abort("spill \{x.to_any()} not found") })
    let offset = frame.spill[slot].or_else(
      fn() { self.tracer.abort("spill offset \{x.to_any()} not found") },
    )
    buf.push(MInsn::Sw(rs=@riscv.reg_swap[i], mem={ base: @riscv.Sp, offset }))
  }
  for i, f in output_freg_spilled {
    let slot = alloc
      .query_spill(f.to_any())
      .or_else(fn() { self.tracer.abort("spill \{f.to_any()} not found") })
    let offset = frame.spill[slot].or_else(
      fn() { self.tracer.abort("spill offset \{f.to_any()} not found") },
    )
    buf.push(
      MInsn::Fsd(rs=@riscv.freg_swap[i], mem={ base: @riscv.Sp, offset }),
    )
  }
}

fn load_to_sp_offset(
  rd : AnyReg,
  offset : @riscv.Imm12,
  insns : Array[MInsn]
) -> Unit {
  match rd.class {
    X => insns.push(MInsn::Ld(rd=rd.to_xreg(), mem={ base: @riscv.Sp, offset }))
    F =>
      insns.push(MInsn::Fld(rd=rd.to_freg(), mem={ base: @riscv.Sp, offset }))
  }
}

fn store_to_sp_offset(
  rs : AnyReg,
  offset : @riscv.Imm12,
  insns : Array[MInsn]
) -> Unit {
  match rs.class {
    X => insns.push(MInsn::Sd(rs=rs.to_xreg(), mem={ base: @riscv.Sp, offset }))
    F =>
      insns.push(MInsn::Fsd(rs=rs.to_freg(), mem={ base: @riscv.Sp, offset }))
  }
}

fn adjust_sp(offset : @riscv.Imm12, insns : Array[MInsn]) -> Unit {
  if offset._ != 0 {
    insns.push(MInsn::Addi(rd=@riscv.Sp, rs1=@riscv.Sp, imm=offset._))
  }
}

fn need_to_be_saved(r : AnyReg, alloc : RegAllocator) -> Bool {
  let r = alloc.query_assigned_color(r).or(r)
  if r.is_phy_freg() || r.is_phy_xreg() {
    match r.class {
      X =>
        @riscv.reg_caller_saved_list.contains(r.to_xreg()) &&
        r != @riscv.reg_ret.to_any()
      F =>
        @riscv.freg_caller_saved_list.contains(r.to_freg()) &&
        r != @riscv.freg_ret.to_any()
    }
  } else {
    false
  }
}

fn RiscvFnLoweringCtx::post_lower_block(
  self : RiscvFnLoweringCtx,
  alloc : RegAllocator,
  mbb : MBasicBlock,
  ibi : IteratedBlockInfo,
  frame : FrameLayout
) -> MBasicBlock {
  let _ = self
  let new_mbb = { id: mbb.id, insns: [], control: [], params: mbb.params }
  let last_save_x = []
  let last_save_f = []
  let insns = new_mbb.insns
  for i, insn in mbb.insns {
    match insn {
      SaveCtx1 => {
        adjust_sp(-frame.adjust_sp, insns)
        for r in frame.function_saves {
          let offset = frame.function_save[r].unwrap()
          store_to_sp_offset(r, offset, insns)
        }
      }
      SaveCtx2 => {
        last_save_f.clear()
        last_save_x.clear()
        // find the call instruction, and get its live set
        // otherwise, we will save more than we need
        let x = {
          let mut x = i + 1
          while true {
            match mbb.insns[x] {
              Call(..) | Jalr(..) => break
              _ => ()
            }
            x += 1
          }
          x
        }
        let live_at_call = ibi.live_set[x + 1]
        for r in live_at_call {
          if need_to_be_saved(r, alloc) {
            match r.class {
              X => last_save_x.push(r)
              F => last_save_f.push(r)
            }
          }
        }
        for i, r in last_save_x {
          let offset = frame.save[i].unwrap()
          store_to_sp_offset(r, offset, insns)
        }
        for i, r in last_save_f {
          let offset = frame.save[i].unwrap()
          store_to_sp_offset(r, offset, insns)
        }
      }
      RestoreCtx2 => {
        for i, r in last_save_f.rev() {
          let offset = frame.save[i].unwrap()
          load_to_sp_offset(r, offset, insns)
        }
        for i, r in last_save_x.rev() {
          let offset = frame.save[i].unwrap()
          load_to_sp_offset(r, offset, insns)
        }
        last_save_x.clear()
        last_save_f.clear()
      }
      _ => self.subst_vreg_with_preg_or_spill(alloc, frame, insn, insns)
    }
  }
  for insn in mbb.control {
    match insn {
      RestoreCtx1 => {
        for r in frame.function_saves {
          let offset = frame.function_save[r].unwrap()
          load_to_sp_offset(r, offset, new_mbb.control)
        }
        adjust_sp(frame.adjust_sp, new_mbb.control)
      }
      _ =>
        self.subst_vreg_with_preg_or_spill(alloc, frame, insn, new_mbb.control)
    }
  }
  new_mbb
}

fn union_array[R : @riscv.ToAnyReg](
  s : MutSet[AnyReg],
  a : Array[R]
) -> MutSet[AnyReg] {
  let result = MutSet::new()
  for r in a {
    if s.contains(r.to_any()) {
      result.insert(r.to_any())
    }
  }
  result
}

struct FrameLayout {
  mut spills : Int
  spill : MutMap[Int, Int] // spill n -> sp - n * 8 - saves.len() * 8 - function_saves.len() * 8
  mut saves : Int
  save : MutMap[Int, Int]
  function_saves : Array[AnyReg] // saved regs for the function
  function_save : MutMap[AnyReg, Int] // save reg -> sp - n * 8
  mut adjust_sp : Int
}

fn FrameLayout::new() -> FrameLayout {
  {
    spills: 0,
    spill: MutMap::new(),
    saves: 0,
    save: MutMap::new(),
    function_saves: [],
    function_save: MutMap::new(),
    adjust_sp: 0,
  }
}

// save a reg for a call
fn FrameLayout::save(self : FrameLayout, num : Int) -> Unit {
  self.saves = @math.maximum(self.saves, num)
}

// save a reg in the prologue of the function
fn FrameLayout::save_at_prologue(self : FrameLayout, reg : AnyReg) -> Unit {
  self.function_saves.push(reg)
}

fn FrameLayout::spill(self : FrameLayout, spilled : Array[AnyReg]) -> Unit {
  self.spills += spilled.length()
}

fn FrameLayout::calculate(self : FrameLayout) -> Unit {
  self.function_saves.sort()
  // reverse because the physical register id is negative and grows downward
  self.function_saves.rev_inplace()
  for i, reg in self.function_saves {
    self.function_save.set(reg, 8 * i)
  }
  let x = self.function_saves.length()
  for i in 0..<self.saves {
    self.save.set(i, 8 * (i + x))
  }
  let y = self.saves
  for i in 0..<self.spills {
    self.spill.set(i, 8 * (i + x + y))
  }
  self.adjust_sp = 8 * (x + y + self.spills)
  if self.adjust_sp % 16 != 0 {
    self.adjust_sp += 8
  }
}

fn RiscvFnLoweringCtx::scan_for_max_lives(
  self : RiscvFnLoweringCtx,
  alloc : RegAllocator,
  mbb : MBasicBlock,
  ibi : IteratedBlockInfo,
  frame : FrameLayout
) -> Unit {
  let _ = self
  for i, insn in mbb.insns {
    match insn {
      SaveCtx2 => {
        // find the call instruction, and get its live set
        // otherwise, we will save more than we need
        let x = {
          let mut x = i + 1
          while true {
            match mbb.insns[x] {
              Call(..) | Jalr(..) => break
              _ => ()
            }
            x += 1
          }
          x
        }
        let live_at_call = ibi.live_set[x + 1]
        let x = live_at_call
          .filter(fn(r) { need_to_be_saved(r, alloc) })
          .length()
        frame.save(x)
      }
      _ => ()
    }
  }
  for insn in mbb.control {
    match insn {
      RestoreCtx2 => self.tracer.abort("RestoreCtx2 should not be in control")
      _ => ()
    }
  }
}

fn RiscvFnLoweringCtx::post_lower(
  self : RiscvFnLoweringCtx,
  alloc : RegAllocator,
  liveness : Liveness
) -> MFn!Failure {
  let mfn = self.mfn.or_error!(Failure("No mfn"))
  let (clobbered_x, clobbered_f) = alloc.get_clobbered_regs()
  let new_mfn = {
    name: mfn.name,
    entry: mfn.entry,
    blocks: MutMap::new(),
    leaf: self.is_leaf,
    seq: mfn.seq,
  }
  // this is accurate
  let x_save = union_array(clobbered_x, @riscv.reg_callee_saved_list)
  let f_save = union_array(clobbered_f, @riscv.freg_callee_saved_list)
  if self.is_leaf == false {
    x_save.insert(@riscv.Ra.to_any())
  }
  // this is a overapproximation w.r.t all calls in the function
  let x_spilled = alloc.query_all_x_spilled()
  let f_spilled = alloc.query_all_f_spilled()
  let frame = FrameLayout::new()
  x_save.each(fn(r) { frame.save_at_prologue(r) })
  f_save.each(fn(r) { frame.save_at_prologue(r) })
  frame.spill(x_spilled)
  frame.spill(f_spilled)
  for block in mfn.blocks {
    let ibi = liveness.iter_block_info
      .get(block.0._)
      .or_error!(Failure("No block info for \{block.0}"))
    self.scan_for_max_lives(alloc, block.1, ibi, frame)
  }
  frame.calculate()
  for block in mfn.blocks {
    let ibi = liveness.iter_block_info
      .get(block.0._)
      .or_error!(Failure("No block info for \{block.0}"))
    let mbb = self.post_lower_block(alloc, block.1, ibi, frame)
    new_mfn.blocks.set(block.0, mbb)
  }
  new_mfn
}

pub fn RiscvFnLoweringCtx::new(
  core : @core.Core,
  func : @core.Fn,
  tracer : @util.SubTracer
) -> RiscvFnLoweringCtx {
  let value_ref_count = MutMap::new()
  let value_regs = MutMap::new()
  // v0 is reserved for the closure pointer
  // we will set it up in assign_regs_for_free_variables
  // NOTE: fv0 is considered to be the closure pointer
  let reg_classes = [RegClass::X, X]
  let closure_reg = @riscv.any_xreg(0)
  let alloc_reg = @riscv.reg_alloc.to_any() // s11 is reserved for the allocator
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
    block_params_assignment: MutMap::new(),
    is_leaf: true,
    mfn: None,
    used_primitive: MutSet::new(),
    tracer,
  }
  lower_ctx.assign_reg_for_closure_ptr()
  lower_ctx.assign_regs_for_function_parameters()
  lower_ctx.assign_regs_for_free_variables()
  lower_ctx.assign_regs_for_mem()
  lower_ctx.assign_regs_for_block_params_and_instrs()
  tracer.println("pre_assigned_registers: \{lower_ctx.value_regs_to_string()}")
  lower_ctx
}

pub struct RiscvLoweringCtx {
  core : @core.Core
  tracer : @util.SubTracer
}

pub fn RiscvLoweringCtx::new(
  core : @core.Core,
  tracer : @util.SubTracer
) -> RiscvLoweringCtx {
  { core, tracer }
}

fn save_sp(insns : Array[MInsn]) -> Unit {
  insns.push(MInsn::Mv(rd=@riscv.Fp, rs=@riscv.Sp))
}

fn restore_sp(insns : Array[MInsn]) -> Unit {
  insns.push(MInsn::Mv(rd=@riscv.Sp, rs=@riscv.Fp))
}

fn pre_allocate(insns : Array[MInsn]) -> Unit {
  insns.push(MInsn::La(rd=@riscv.reg_alloc, l=Label("large_heap_end")))
}

fn pre_allocate_for_stack(insns : Array[MInsn]) -> Unit {
  insns.push(MInsn::La(rd=@riscv.Sp, l=Label("large_stack_end")))
}

pub fn RiscvLoweringCtx::lower_globals(
  self : RiscvLoweringCtx,
  globals : MutMap[@riscv.Label, Int]
) -> MFn {
  let core = self.core
  let name = @riscv.Label::Label("minimbt_main")
  let entry = @riscv.Label::Label(".minimbt_main_b0")
  let blocks = MutMap::new()
  let insns = []
  let save_list = [@riscv.Ra, @riscv.Fp, @riscv.S11]
  let mut len = save_list.length() * 8
  if len % 16 != 0 {
    len += 8
  }
  adjust_sp(-len, insns)
  for i, reg in save_list {
    store_to_sp_offset(reg.to_any(), i * 8, insns)
  }
  pre_allocate(insns)
  save_sp(insns)
  pre_allocate_for_stack(insns)
  for symbol in core.global_names {
    let symbol = core.global_symbols.get(symbol.1).unwrap()
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
    let call = MInsn::Call(
      target=Label(thunk.name.to_string()),
      num_xregs=0,
      num_fregs=0,
    )
    insns.push(call)
    insns.push(
      MInsn::La(rd=@riscv.reg_swap[0], l=Label(symbol.name.to_string())),
    )
    match (ty_size, rc) {
      (4, X) =>
        insns.push(
          MInsn::Sw(
            rs=@riscv.reg_ret,
            mem={ base: @riscv.reg_swap[0], offset: 0 },
          ),
        )
      (8, X) =>
        insns.push(
          MInsn::Sd(
            rs=@riscv.reg_ret,
            mem={ base: @riscv.reg_swap[0], offset: 0 },
          ),
        )
      (8, F) =>
        insns.push(
          MInsn::Fsd(rs=@riscv.Fa0, mem={ base: @riscv.reg_swap[0], offset: 0 }),
        )
      (0, _) => {
        let _ = insns.unsafe_pop()

      }
      _ => self.tracer.abort("Invalid thunk type size")
    }
  }
  restore_sp(insns)
  for i, reg in save_list {
    load_to_sp_offset(reg.to_any(), i * 8, insns)
  }
  adjust_sp(len, insns)
  let control = [MInsn::XRet]
  let params = []
  blocks.set(entry, { id: entry, insns, control, params })
  { name, entry, blocks, leaf: false, seq: [entry] }
}

pub fn RiscvLoweringCtx::lower(self : RiscvLoweringCtx) -> MCore!Failure {
  let globals = MutMap::new()
  let before_colored = {
    funcs: MutMap::new(),
    globals,
    liveness: MutMap::new(),
    primitives: MutSet::new(),
  }
  let after_colored = {
    funcs: MutMap::new(),
    globals,
    liveness: MutMap::new(),
    primitives: MutSet::new(),
  }
  {
    let init = self.lower_globals(globals)
    before_colored.funcs.set(init.name, init)
    after_colored.funcs.set(init.name, init)
    let l = Liveness::new(None, init)
    l.compute_block_liveness()
    before_colored.liveness.set(init.name, l)
  }
  for func in self.core.funcs {
    let (_, func) = func
    let lower_ctx = RiscvFnLoweringCtx::new(self.core, func, self.tracer)
    let (mfn, liveness) = lower_ctx.lower!()
    let lbl = @riscv.Label::Label(func.name)
    before_colored.funcs.set(lbl, mfn)
    before_colored.liveness.set(lbl, liveness)
    lower_ctx.used_primitive.each(
      fn(p) {
        before_colored.primitives.insert(p)
        after_colored.primitives.insert(p)
      },
    )
    let regalloc = RegAllocator::new(mfn, liveness, self.tracer)
    regalloc.assign()
    let final_mfn = lower_ctx.post_lower!(regalloc, liveness)
    after_colored.funcs.set(lbl, final_mfn)
  }
  self.tracer.println(
    "before colored: \{Show::to_string(before_colored.to_rich_info())}",
  )
  self.tracer.record("mcore", after_colored)
  after_colored
}
