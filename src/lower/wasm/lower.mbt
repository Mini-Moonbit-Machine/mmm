typealias MutMap[K, V] = @hashmap.T[K, V]

typealias MutSet[K] = @hashset.T[K]

fn type_mangling(ty : @core.Type) -> String {
  match ty {
    Unit => "u"
    Bool => "b"
    Int32 => "i32"
    Int64 => "i64"
    Double => "f64"
    ClosureFn(~params, ~ty) => {
      let params_str = params.map(fn(p) { type_mangling(p) }).join("_")
      let ret_str = type_mangling(ty)
      "closure_\{params_str}->\{ret_str}"
    }
    DirectFn(~params, ~ty) => {
      let params_str = params.map(fn(p) { type_mangling(p) }).join("_")
      let ret_str = type_mangling(ty)
      "fn_\{params_str}->\{ret_str}"
    }
    Array(..) => "arr"
    _ => "unknown"
  }
}

pub struct WasmLoweringCtx {
  core : @core.Core
  tracer : @util.SubTracer
}

pub fn WasmLoweringCtx::new(
  core : @core.Core,
  tracer : @util.SubTracer
) -> WasmLoweringCtx {
  WasmLoweringCtx::{ core, tracer }
}

pub struct WasmFnLoweringCtx {
  core : @core.Core
  func : @core.Fn
  order : @lower.LoweringOrder
  buffer : Array[WasmInsn]
  icache : Array[WasmInsn]
  // type sections
  func_type_ctx : MutMap[Label, (Array[Type], Type?)]
  // locals
  local_reg : MutMap[@core.Value, Name]
  branch_aux_reg : Name
  this_ptr : Name
  // globals
  closure_ptr : Name
  closure_table : Label
  closure_table_index : MutMap[@core.FnRef, TableIndex]
  tracer : @util.SubTracer
}

fn WasmFnLoweringCtx::type_of(
  self : WasmFnLoweringCtx,
  v : @core.Value
) -> @core.Type {
  let ty = try {
    v.ty!(self.core, self.func)
  } catch {
    Failure(msg) => self.tracer.abort(msg)
  }
  ty
}

fn ty_to_wasm_type(ty : @core.Type) -> Type? {
  match ty {
    Unit => None
    Double => Some(F64)
    _ => Some(I32)
  }
}

fn WasmFnLoweringCtx::get_local_name(
  self : WasmFnLoweringCtx,
  value : @core.Value
) -> Name {
  let name = match value {
    Unit | Bool(..) | Int32(..) | Int64(..) | Double(..) | Fn(..) =>
      self.tracer.abort("value should not have a local name")
    Insn(~iref) =>
      self.local_reg
      .get(value)
      .or_else(
        fn() {
          let name = Name("\{iref}")
          self.local_reg.set(value, name)
          name
        },
      )
    Mem(~fref, ~mref) =>
      self.local_reg
      .get(value)
      .or_else(
        fn() {
          let name = Name("\{fref}.\{mref}")
          self.local_reg.set(value, name)
          name
        },
      )
    FreeVar(~fvref) =>
      self.local_reg
      .get(value)
      .or_else(
        fn() {
          let name = Name("\{fvref}")
          self.local_reg.set(value, name)
          name
        },
      )
    BlockParam(~bref, ~bpref) =>
      self.local_reg
      .get(value)
      .or_else(
        fn() {
          let name = Name("\{bref}.\{bpref}")
          self.local_reg.set(value, name)
          name
        },
      )
    FnParam(~fpref, ..) => Name("\{fpref}")
    Self(..) => self.closure_ptr
  }
  self.tracer.println("Get Local name \{value} -> \{name}")
  name
}

pub fn WasmFnLoweringCtx::emit_wasm(
  self : WasmFnLoweringCtx,
  insn : WasmInsn
) -> Unit {
  self.icache.push(insn)
}

pub fn WasmFnLoweringCtx::lower_value(
  self : WasmFnLoweringCtx,
  value : @core.Value
) -> Unit {
  match value {
    @core.Value::Unit => self.emit_wasm(Comment(comment="unit value"))
    @core.Value::Bool(~val) =>
      match val {
        true => self.emit_wasm(I32Const(val=1))
        false => self.emit_wasm(I32Const(val=0))
      }
    @core.Value::Double(~val) => self.emit_wasm(F64Const(~val))
    @core.Value::Int32(~val) => self.emit_wasm(I32Const(~val))
    @core.Value::Int64(..) => self.tracer.abort("Int64 is not supported")
    @core.Value::Insn(..) =>
      self.emit_wasm(GetLocal(name=self.get_local_name(value)))
    @core.Value::Fn(~fref) => {
      // get the function reference index in the table
      let table_index = self.closure_table_index
        .get(fref)
        .or_else(fn() { self.tracer.abort("Function not found") })
      self.emit_wasm(I32Const(val=table_index._))
    }
    @core.Value::Self(..) => self.emit_wasm(GetGlobal(name=self.closure_ptr))
    @core.Value::Mem(~mref, ..) => {
      let mem = self.func.mems
        .get(mref)
        .or_else(fn() { self.tracer.abort("Memory not found") })
      match mem {
        Stack(..) =>
          self.tracer.abort("Stack memory should not appear in wasm backend")
        Heap(..) => self.emit_wasm(GetLocal(name=self.get_local_name(value)))
      }
    }
    @core.Value::BlockParam(..) =>
      self.emit_wasm(GetLocal(name=self.get_local_name(value)))
    @core.Value::FnParam(..) =>
      self.emit_wasm(GetLocal(name=self.get_local_name(value)))
    @core.Value::FreeVar(..) =>
      self.emit_wasm(GetLocal(name=self.get_local_name(value)))
  }
}

pub fn WasmFnLoweringCtx::lower_address(
  self : WasmFnLoweringCtx,
  addr : @core.Address
) -> Unit {
  match addr {
    Raw(..) =>
      self.tracer.abort("Raw address should not appear in wasm backend")
    Field(~val, ~index, ~width, ..) => {
      match self.type_of(val) {
        ClosureFn(..) =>
          self.tracer.abort(
            "Try to get a field inside a closure, this should not happen",
          )
        _ => ()
      }
      // TODO: tuple element size
      let offset = index * width.size_of_ilp32()
      self.lower_value(val)
      self.emit_wasm(I32Const(val=offset))
      self.emit_wasm(I32Add)
    }
    Offset(~val, ~offset, ~width, ..) => {
      match self.type_of(val) {
        ClosureFn(..) =>
          self.tracer.abort(
            "Try to get a offset inside a closure, this should not happen",
          )
        _ => ()
      }
      self.lower_value(val)
      self.lower_value(offset)
      match width.size_of_ilp32() {
        4 => {
          self.emit_wasm(I32Const(val=2))
          self.emit_wasm(I32Sll)
        }
        8 => {
          self.emit_wasm(I32Const(val=3))
          self.emit_wasm(I32Sll)
        }
        _ => self.tracer.abort("Invalid type size")
      }
      self.emit_wasm(I32Add)
    }
  }
}

pub fn WasmFnLoweringCtx::set_local_variable(
  self : WasmFnLoweringCtx,
  value : @core.Value
) -> Unit {
  match self.type_of(value) {
    Unit => ()
    _ => {
      let name = self.get_local_name(value)
      self.emit_wasm(Comment(comment="set \{name}"))
      self.emit_wasm(SetLocal(~name))
    }
  }
}

pub fn WasmFnLoweringCtx::set_none_unit_call_args(
  self : WasmFnLoweringCtx,
  args : Array[@core.Value]
) -> Unit {
  for arg in args {
    match self.type_of(arg) {
      Unit => ()
      _ => self.lower_value(arg)
    }
  }
}

pub fn WasmFnLoweringCtx::set_none_unit_block_args(
  self : WasmFnLoweringCtx,
  target : @core.BlockRef,
  args : Array[@core.Value]
) -> Unit {
  let target = self.target_bb(target)
  for i, arg in args.iter2() {
    match self.type_of(arg) {
      Unit => ()
      _ => {
        self.lower_value(arg)
        self.set_local_variable(@core.block_param(target, i))
      }
    }
  }
}

pub fn WasmFnLoweringCtx::lower_insn(
  self : WasmFnLoweringCtx,
  insnref : @core.InsnRef,
  insn : @core.Insn
) -> Unit {
  self.tracer.println("Lowering insn \{insn}")
  match insn {
    @core.Insn::Op(~ty, ~op, ~args) => {
      match ty {
        @core.Type::Bool =>
          match op {
            @core.Op::Lnot => {
              self.lower_value(args[0])
              self.emit_wasm(I32Eqz)
            }
            @core.Op::Eq => {
              self.lower_value(args[0])
              self.lower_value(args[1])
              let ty = self.type_of(args[0])
              match ty {
                @core.Type::Bool => self.emit_wasm(I32Eq)
                @core.Type::Int32 => self.emit_wasm(I32Eq)
                @core.Type::Double => self.emit_wasm(F64Eq)
                _ => self.tracer.abort("Unsupported type operand of Eq")
              }
            }
            @core.Op::Le => {
              self.lower_value(args[0])
              self.lower_value(args[1])
              let ty = self.type_of(args[0])
              match ty {
                @core.Type::Int32 => self.emit_wasm(I32Le)
                @core.Type::Double => self.emit_wasm(F64Le)
                _ => self.tracer.abort("Unsupported type operand of Le")
              }
            }
            _ => self.tracer.abort("Unsupported bool operation")
          }
        @core.Type::Int32 =>
          match op {
            @core.Op::Add => {
              self.lower_value(args[0])
              self.lower_value(args[1])
              self.emit_wasm(I32Add)
            }
            @core.Op::Sub => {
              self.lower_value(args[0])
              self.lower_value(args[1])
              self.emit_wasm(I32Sub)
            }
            @core.Op::Mul => {
              self.lower_value(args[0])
              self.lower_value(args[1])
              self.emit_wasm(I32Mul)
            }
            @core.Op::Div => {
              self.lower_value(args[0])
              self.lower_value(args[1])
              self.emit_wasm(I32Div)
            }
            @core.Op::Neg => {
              self.emit_wasm(I32Const(val=0))
              self.lower_value(args[0])
              self.emit_wasm(I32Sub)
            }
            @core.Op::D2I(..) => {
              self.lower_value(args[0])
              self.emit_wasm(I32TruncF64)
            }
            _ => self.tracer.abort("Unsupported int32 operation")
          }
        @core.Type::Double =>
          match op {
            @core.Op::Add => {
              self.lower_value(args[0])
              self.lower_value(args[1])
              self.emit_wasm(F64Add)
            }
            @core.Op::Sub => {
              self.lower_value(args[0])
              self.lower_value(args[1])
              self.emit_wasm(F64Sub)
            }
            @core.Op::Mul => {
              self.lower_value(args[0])
              self.lower_value(args[1])
              self.emit_wasm(F64Mul)
            }
            @core.Op::Div => {
              self.lower_value(args[0])
              self.lower_value(args[1])
              self.emit_wasm(F64Div)
            }
            @core.Op::Neg => {
              self.lower_value(args[0])
              self.emit_wasm(F64Neg)
            }
            @core.Op::I2D(..) => {
              self.lower_value(args[0])
              self.emit_wasm(F64CvtI32)
            }
            @core.Op::Abs => {
              self.lower_value(args[0])
              self.emit_wasm(F64Abs)
            }
            @core.Op::Sqrt => {
              self.lower_value(args[0])
              self.emit_wasm(F64Sqrt)
            }
            _ => self.tracer.abort("Unsupported double operation")
          }
        _ => self.tracer.abort("Unsupported type")
      }
      self.set_local_variable(@core.Value::Insn(iref=insnref))
    }
    @core.Insn::Call(~f, ~args, ..) => {
      self.set_none_unit_call_args(args)
      let func = self.core.funcs
        .get(f)
        .or_else(fn() { self.tracer.abort("Function \{f} not found") })
      self.emit_wasm(Call(target=Label(func.name)))
      self.set_local_variable(@core.Value::Insn(iref=insnref))
    }
    @core.Insn::ExtCall(~ext_f, ~args, ..) => {
      self.set_none_unit_call_args(args)
      self.emit_wasm(Call(target=Label(ext_f)))
      self.set_local_variable(@core.Value::Insn(iref=insnref))
    }
    @core.Insn::LoadSymbol(~sref, ..) => {
      let symbol = self.core.global_symbols
        .get(sref)
        .or_else(fn() { self.tracer.abort("Symbol not found") })
      self.emit_wasm(GetGlobal(name=Name(symbol.name)))
      self.set_local_variable(@core.Value::Insn(iref=insnref))
    }
    @core.Insn::Alloc(~mref, ~fills, ..) => {
      let mem = self.func.mems
        .get(mref)
        .or_else(fn() { self.tracer.abort("Memory not found") })
      match mem {
        Stack(..) =>
          self.tracer.abort("Stack memory should not appear in wasm backend")
        Heap(~len, ~ty) => {
          let elem_size = ty.elem_size_of_ilp32()
          guard elem_size > 0 else {
            self.tracer.abort("Zero size type")
            // do fills
          }
          let mut allocated = false
          for fill in fills {
            match fill {
              Zero => {
                guard fills.length() == 1 else {
                  self.tracer.abort("Zero fill should be the only fill")
                }
                match ty.elem_size_of_ilp32() {
                  4 => {
                    self.lower_value(len)
                    self.emit_wasm(I32Const(val=0))
                    self.emit_wasm(Call(target=Label("minimbt_create_array")))
                    self.set_local_variable(
                      @core.Value::Mem(fref=self.func.id, ~mref),
                    )
                  }
                  8 => {
                    self.lower_value(len)
                    self.emit_wasm(F64Const(val=0))
                    self.emit_wasm(
                      Call(target=Label("minimbt_create_float_array")),
                    )
                    self.set_local_variable(
                      @core.Value::Mem(fref=self.func.id, ~mref),
                    )
                  }
                  _ => self.tracer.abort("Uncompatible zero fill \{ty}")
                }
              }
              Init(~val) => {
                guard fills.length() == 1 else {
                  self.tracer.abort("Init fill should be the only fill")
                }
                match self.type_of(val) {
                  Double => {
                    self.lower_value(len)
                    self.lower_value(val)
                    self.emit_wasm(
                      Call(target=Label("minimbt_create_float_array")),
                    )
                    self.set_local_variable(
                      @core.Value::Mem(fref=self.func.id, ~mref),
                    )
                  }
                  _ => {
                    self.lower_value(len)
                    self.lower_value(val)
                    self.emit_wasm(Call(target=Label("minimbt_create_array")))
                    self.set_local_variable(
                      @core.Value::Mem(fref=self.func.id, ~mref),
                    )
                  }
                }
              }
              Field(~index, ~val) => {
                if not(allocated) {
                  let imm_len = match len {
                    Int32(~val) => val
                    _ => self.tracer.abort("Not supported alloc length imm")
                  }
                  let size = imm_len * elem_size
                  self.emit_wasm(I32Const(val=size))
                  self.emit_wasm(Call(target=Label("minimbt_malloc")))
                  self.set_local_variable(
                    @core.Value::Mem(fref=self.func.id, ~mref),
                  )
                  allocated = true
                }
                self.emit_wasm(
                  GetLocal(
                    name=self.get_local_name(
                      @core.Value::Mem(fref=self.func.id, ~mref),
                    ),
                  ),
                )
                let offset = index * elem_size
                self.emit_wasm(I32Const(val=offset))
                self.emit_wasm(I32Add)
                self.lower_value(val)
                match self.type_of(val) {
                  Double => self.emit_wasm(F64Store)
                  _ => self.emit_wasm(I32Store)
                }
              }
            }
          }
        }
      }
    }
    @core.Insn::Store(~ty, ~ma, ~val) => {
      match self.type_of(val) {
        Unit => self.tracer.abort("Unit value should not be stored")
        _ => ()
      }
      self.lower_address(ma)
      self.lower_value(val)
      match ty_to_wasm_type(ty) {
        None => self.tracer.abort("Unit value should not be stored")
        Some(I32) => self.emit_wasm(I32Store)
        Some(F64) => self.emit_wasm(F64Store)
        _ => self.tracer.abort("Unsupported store type")
      }
    }
    @core.Insn::Load(~ty, ~ma) => {
      self.lower_address(ma)
      match ty_to_wasm_type(ty) {
        None => self.tracer.abort("Unit value should not be loaded")
        Some(I32) => self.emit_wasm(I32Load)
        Some(F64) => self.emit_wasm(F64Load)
        _ => self.tracer.abort("Unexpected load type")
      }
      self.set_local_variable(@core.Value::Insn(iref=insnref))
    }
    @core.Insn::Apply(~f, ~args, ..) => {
      // save context
      self.emit_wasm(GetGlobal(name=self.closure_ptr))
      self.emit_wasm(SetLocal(name=self.this_ptr))
      // get closure ptr
      self.lower_value(f)
      self.emit_wasm(SetGlobal(name=self.closure_ptr))
      // closure args
      self.set_none_unit_call_args(args)
      // closure value
      let closure_ty = self.type_of(f)
      let closure_ty_name = type_mangling(closure_ty)
      // update function type context
      guard let ClosureFn(~params, ~ty) = closure_ty else {
        _ => self.tracer.abort("Invalid closure type")
      }
      let params_ty = []
      for param in params {
        match ty_to_wasm_type(param) {
          None => ()
          Some(ty) => params_ty.push(ty)
        }
      }
      let ret_ty = ty_to_wasm_type(ty)
      self.func_type_ctx.set(Label(closure_ty_name), (params_ty, ret_ty))
      self.lower_value(f)
      self.emit_wasm(I32Load)
      self.emit_wasm(CallIndirect(typename=Label(closure_ty_name)))
      self.set_local_variable(@core.Value::Insn(iref=insnref))
      // restore context
      self.emit_wasm(GetLocal(name=self.this_ptr))
      self.emit_wasm(SetGlobal(name=self.closure_ptr))
    }
    ComputeAddress(..) =>
      self.tracer.abort("ComputeAddress should not appear in wasm backend")
  }
}

pub fn WasmFnLoweringCtx::target_bb(
  self : WasmFnLoweringCtx,
  target : @core.BlockRef
) -> @core.BlockRef {
  let target = self.func.blocks.get(target).unwrap()
  let target = target.bb_or_header_bb(self.func).id
  target
}

pub fn WasmFnLoweringCtx::lower_control(
  self : WasmFnLoweringCtx,
  control : @core.Control,
  branch_table_index : MutMap[@core.BlockRef, LabelIndex]
) -> Unit {
  self.tracer.println("Lowering control \{control}")
  match control {
    @core.Control::Return(~args) => {
      self.set_none_unit_call_args(args)
      self.emit_wasm(Return)
    }
    @core.Control::Jump(~target, ~args) => {
      let target = self.target_bb(target)
      self.set_none_unit_block_args(target, args)
      let target_index = branch_table_index
        .get(target)
        .or_else(fn() { self.tracer.abort("Target not found") })
      self.emit_wasm(I32Const(val=target_index._))
      self.emit_wasm(SetLocal(name=self.branch_aux_reg))
      self.emit_wasm(Br(target="loop%\{self.func.name}"))
    }
    @core.Control::Branch(~cond, ~t, ~t_args, ~f, ~f_args) => {
      let t = self.target_bb(t)
      let f = self.target_bb(f)
      self.lower_value(cond)
      self.emit_wasm(If)
      let t_index = branch_table_index
        .get(t)
        .or_else(fn() { self.tracer.abort("Target not found") })
      self.set_none_unit_block_args(t, t_args)
      self.emit_wasm(I32Const(val=t_index._))
      self.emit_wasm(SetLocal(name=self.branch_aux_reg))
      self.emit_wasm(Br(target="loop%\{self.func.name}"))
      self.emit_wasm(Else)
      let f_index = branch_table_index
        .get(f)
        .or_else(fn() { self.tracer.abort("Target not found") })
      self.set_none_unit_block_args(f, f_args)
      self.emit_wasm(I32Const(val=f_index._))
      self.emit_wasm(SetLocal(name=self.branch_aux_reg))
      self.emit_wasm(Br(target="loop%\{self.func.name}"))
      self.emit_wasm(End)
    }
    _ => self.tracer.abort("Unsupported control")
  }
}

pub fn WasmFnLoweringCtx::lower_block(
  self : WasmFnLoweringCtx,
  block : @core.BasicBlock,
  branch_table_index : MutMap[@core.BlockRef, LabelIndex]
) -> Unit {
  for insnref in block.seq {
    let insn = self.func.insns
      .get(insnref)
      .or_else(fn() { self.tracer.abort("Instruction not found") })
    self.lower_insn(insnref, insn)
  }
  self.lower_control(block.control, branch_table_index)
  // finalize
  self.emit_wasm(Comment(comment="finalize block \{block.id}"))
  self.emit_wasm(End)
  self.buffer.append(self.icache)
  self.icache.clear()
}

pub fn WasmFnLoweringCtx::emit_prologue(self : WasmFnLoweringCtx) -> Unit {
  // store free variables
  self.emit_wasm(Comment(comment="store free variables"))
  let ty = self.func.ty
  let elem_size = ty.elem_size_of_ilp32()
  for i, fvar in self.func.fvars.iter2() {
    let name = self.get_local_name(
      @core.Value::FreeVar(fvref=@core.FnFreeVarRef({ id: i + 1 })),
    )
    self.emit_wasm(GetGlobal(name=self.closure_ptr))
    self.emit_wasm(I32Const(val=(i + 1) * elem_size))
    self.emit_wasm(I32Add)
    match ty_to_wasm_type(fvar) {
      None => self.tracer.abort("Unit type should not appear in free variables")
      Some(F64) => self.emit_wasm(F64Load)
      Some(I32) => self.emit_wasm(I32Load)
      _ => self.tracer.abort("Invalid free variable type")
    }
    self.emit_wasm(SetLocal(~name))
  }
  self.buffer.append(self.icache)
  self.icache.clear()
}

pub fn WasmFnLoweringCtx::lower(self : WasmFnLoweringCtx) -> MFn {
  self.tracer.println("Lowering function \{self.func.name}")
  // compute branch table index
  let branch_table_index = MutMap::new()
  let order = self.order.order.rev()
  let branch_index = []
  for i, block in order.iter2() {
    match block {
      BasicBlock(~bb) => {
        branch_table_index.set(bb.id, LabelIndex(i))
        branch_index.push(LabelIndex(i))
      }
      _ => self.tracer.abort("Critical edge not supported")
    }
  }
  // set full through branch
  // branch_index.push(LabelIndex(order.length()))

  // prologue: store free variables
  self.emit_prologue()

  // set up blocks
  self.buffer.push(
    I32Const(
      val=branch_table_index
        .get(self.func.entry)
        .or_else(fn() { self.tracer.abort("Entry not found") })._,
    ),
  )
  self.buffer.push(SetLocal(name=self.branch_aux_reg))
  self.buffer.push(Loop(label="loop%\{self.func.name}"))
  for block in order {
    match block {
      BasicBlock(..) => self.buffer.push(Block)
      _ => self.tracer.abort("Critical edge not supported")
    }
  }

  // switch
  self.buffer.push(Block)
  self.buffer.push(GetLocal(name=self.branch_aux_reg))
  self.buffer.push(BrTable(targets=branch_index))
  self.buffer.push(End)
  for block in order {
    match block {
      BasicBlock(~bb) => {
        self.emit_wasm(Comment(comment="block \{bb.id}"))
        self.lower_block(bb, branch_table_index)
      }
      _ => self.tracer.abort("Critical edge not supported")
    }
  }
  self.buffer.push(Comment(comment="end loop"))
  self.buffer.push(End)
  // TODO: workaround for wasm validation
  guard let ClosureFn(~params, ~ty) | DirectFn(~params, ~ty) = self.func.ty else {
    _ => self.tracer.abort("Invalid function type")
  }
  match ty_to_wasm_type(ty) {
    None => ()
    Some(I32) => self.buffer.push(I32Const(val=0))
    Some(F64) => self.buffer.push(F64Const(val=0.0))
    _ => self.tracer.abort("Invalid return type")
  }

  // construct MFn
  let args = []
  for i, param in params.iter2() {
    match ty_to_wasm_type(param) {
      None => ()
      Some(ty) => {
        let name = self.get_local_name(@core.fn_param(self.func.id, i))
        args.push((name, ty))
      }
    }
  }
  let ret = ty_to_wasm_type(ty)
  let locals = [(self.branch_aux_reg, I32), (self.this_ptr, I32)]
  for vnpair in self.local_reg {
    let (value, name) = vnpair
    self.tracer.println("Local \{value} -> \{name}")
    let ty = self.type_of(value)
    locals.push(
      (
        name,
        ty_to_wasm_type(ty).or_else(
          fn() {
            self.tracer.abort("function locals should not have unit type")
          },
        ),
      ),
    )
  }
  MFn::{ name: self.func.name, args, ret, locals, seq: self.buffer }
}

pub fn WasmLoweringCtx::lower_globals(
  self : WasmLoweringCtx,
  globals : MutMap[Name, Type]
) -> MFn {
  let name = Label("minimbt_main")
  let insn = []
  for symbol in self.core.global_names {
    let symbol = self.core.global_symbols.get(symbol.1).unwrap()
    match symbol.thunk {
      Some(thunk) => {
        let thunk = self.core.funcs.get(thunk).unwrap()
        insn.push(Call(target=Label(thunk.name)))
        match ty_to_wasm_type(symbol.ty) {
          None => ()
          Some(I32) => {
            globals.set(Name(symbol.name), I32)
            insn.push(SetGlobal(name=Name(symbol.name)))
          }
          Some(F64) => {
            globals.set(Name(symbol.name), F64)
            insn.push(SetGlobal(name=Name(symbol.name)))
          }
          _ => self.tracer.abort("Invalid global type")
        }
      }
      None =>
        match symbol.data {
          Some(@core.Value::Unit) => {
            insn.push(I32Const(val=0))
            globals.set(Name(symbol.name), I32)
            insn.push(SetGlobal(name=Name(symbol.name)))
          }
          Some(@core.Value::Bool(~val)) => {
            insn.push(I32Const(val=if val { 1 } else { 0 }))
            globals.set(Name(symbol.name), I32)
            insn.push(SetGlobal(name=Name(symbol.name)))
          }
          Some(@core.Value::Int32(~val)) => {
            insn.push(I32Const(~val))
            globals.set(Name(symbol.name), I32)
            insn.push(SetGlobal(name=Name(symbol.name)))
          }
          Some(@core.Value::Double(~val)) => {
            insn.push(F64Const(~val))
            globals.set(Name(symbol.name), F64)
            insn.push(SetGlobal(name=Name(symbol.name)))
          }
          Some(_) => self.tracer.abort("Invalid global symbol")
          None => self.tracer.abort("Invalid global symbol")
        }
    }
  }
  let start_fn = MFn::{ name, args: [], ret: None, locals: [], seq: insn }
  start_fn
}

pub fn WasmLoweringCtx::lower(self : WasmLoweringCtx) -> MCore {
  // closure table
  let closure_table_index = MutMap::new()
  let mut i = 0
  for fpair in self.core.funcs {
    let (fref, _) = fpair
    closure_table_index.set(fref, TableIndex(i))
    i = i + 1
  }
  // lowering functions.
  let mfuncs = []
  let func_type_ctx = MutMap::new()
  for fpair in self.core.funcs {
    let (_, func) = fpair
    let order = @lower.LoweringOrder::new(func, self.tracer)
    let ctx = WasmFnLoweringCtx::{
      core: self.core,
      func,
      order,
      buffer: [],
      icache: [],
      func_type_ctx,
      local_reg: MutMap::new(),
      branch_aux_reg: Name("branch_aux_reg"),
      this_ptr: Name("this_ptr"),
      closure_ptr: Name("closure_ptr"),
      closure_table: Label("closure_table"),
      closure_table_index,
      tracer: self.tracer,
    }
    let mfn = ctx.lower()
    mfuncs.push(mfn)
    self.tracer.log("Lowered function \{func.name} to wasm")
  }

  // globals
  let globals = MutMap::of([(Name("closure_ptr"), I32)])
  let start_fn = self.lower_globals(globals)
  mfuncs.push(start_fn)
  // tables
  let closure_reverse_index = MutMap::new()
  for fipair in closure_table_index {
    let (fref, index) = fipair
    let func = self.core.funcs.get(fref).unwrap()
    closure_reverse_index.set(index, Label(func.name))
  }
  let elems = []
  for i in 0..<self.core.funcs.size() {
    let label = closure_reverse_index.get(TableIndex(i)).unwrap()
    elems.push(label)
  }
  let table = MTable::{
    name: Label("closure_table"),
    size: self.core.funcs.size(),
    elems,
  }
  MCore::{ func_type_ctx, table, globals, mfuncs }
}
