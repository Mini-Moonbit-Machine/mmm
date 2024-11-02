pub typealias MutMap[K, V] = @hashmap.T[K, V]

pub struct Id {
  id : Int
} derive(Eq, Hash, Show)

pub fn Id::new(id : Int) -> Id {
  { id, }
}

pub type FnRef Id derive(Eq, Hash)

pub type BlockRef Id derive(Eq, Hash)

pub type InsnRef Id derive(Eq, Hash)

pub type BlockParamRef Id derive(Eq, Hash)

pub type FnParamRef Id derive(Eq, Hash)

pub type FnFreeVarRef Id derive(Eq, Hash)

pub type MemRef Id derive(Eq, Hash)

pub type SymbolRef Id derive(Eq, Hash)

pub typealias Values = Array[Value]

pub typealias Fills = Array[Fill]

pub typealias Types = Array[Type]

pub fn output(self : FnRef, logger : Logger) -> Unit {
  logger.write_char('f')
  logger.write_object(self.id)
}

pub fn to_string(self : FnRef) -> String {
  Show::to_string(self)
}

pub fn output(self : BlockRef, logger : Logger) -> Unit {
  logger.write_char('b')
  logger.write_object(self.id)
}

pub fn to_string(self : BlockRef) -> String {
  Show::to_string(self)
}

pub fn output(self : InsnRef, logger : Logger) -> Unit {
  logger.write_char('%')
  logger.write_object(self.id)
}

pub fn to_string(self : InsnRef) -> String {
  Show::to_string(self)
}

pub fn output(self : BlockParamRef, logger : Logger) -> Unit {
  logger.write_string("$b.")
  logger.write_object(self.id)
}

pub fn to_string(self : BlockParamRef) -> String {
  Show::to_string(self)
}

pub fn output(self : FnParamRef, logger : Logger) -> Unit {
  logger.write_string("$f.")
  logger.write_object(self.id)
}

pub fn to_string(self : FnParamRef) -> String {
  Show::to_string(self)
}

pub fn output(self : MemRef, logger : Logger) -> Unit {
  logger.write_char('m')
  logger.write_object(self.id)
}

pub fn to_string(self : MemRef) -> String {
  Show::to_string(self)
}

pub fn output(self : FnFreeVarRef, logger : Logger) -> Unit {
  logger.write_char('^')
  logger.write_object(self.id)
}

pub fn to_string(self : FnFreeVarRef) -> String {
  Show::to_string(self)
}

pub fn output(self : SymbolRef, logger : Logger) -> Unit {
  logger.write_string("::")
  logger.write_object(self.id)
}

pub fn to_string(self : SymbolRef) -> String {
  Show::to_string(self)
}

pub struct Core {
  names : MutMap[String, FnRef]
  funcs : MutMap[FnRef, Fn]
  global_names : Array[(String, SymbolRef)]
  global_symbols : MutMap[SymbolRef, Symbol]
} derive(Show)

pub struct Fn {
  id : FnRef
  name : String
  ty : Type
  fvars : Array[Type]
  mut entry : BlockRef
  blocks : MutMap[BlockRef, Block]
  insns : MutMap[InsnRef, Insn]
  mems : MutMap[MemRef, Mem]
} derive(Show)

pub struct BasicBlock {
  id : BlockRef
  params : Array[Type]
  seq : Array[InsnRef]
  mut control : Control
} derive(Show)

pub struct Loop {
  id : BlockRef
  mut parent : BlockRef?
  mut header : BlockRef // must be a basic block
  blocks : Array[BlockRef]
} derive(Show, Eq)

pub enum Block {
  BasicBlock(~bb : BasicBlock)
  Loop(~l : Loop)
} derive(Show)

pub struct Symbol {
  id : SymbolRef
  name : String
  ty : Type
  thunk : FnRef
} derive(Show)

pub enum Type {
  Unit
  Bool
  Int32
  Int64
  Double
  ClosureFn(~params : Array[Type], ~ty : Type) // (fn_ptr, closure_args...)
  DirectFn(~params : Array[Type], ~ty : Type) // fn_ptr itself
  Tuple(~fields : Array[Type])
  Array(~elem : Type)
  Ptr
} derive(Show, Eq)

pub enum Value {
  Unit
  Bool(~val : Bool)
  Int32(~val : Int)
  Int64(~val : Int64)
  Double(~val : Double)
  Insn(~iref : InsnRef)
  Mem(~fref : FnRef, ~mref : MemRef)
  Fn(~fref : FnRef)
  FreeVar(~fvref : FnFreeVarRef)
  BlockParam(~bref : BlockRef, ~bpref : BlockParamRef)
  FnParam(~fref : FnRef, ~fpref : FnParamRef)
  Self(~fref : FnRef)
} derive(Show, Eq)

pub enum RoundMode {
  Rne
  Rtz
  Rdn
  Rup
  Rmm
} derive(Eq)

pub enum Op {
  Add
  Sub
  Mul
  Div
  Neg
  Lnot
  Eq
  Le
  I2D
  D2I(~rm : RoundMode)
  Abs
  Sqrt
} derive(Show, Eq)

pub enum Insn {
  Op(~ty : Type, ~op : Op, ~args : Values)
  Call(~ty : Type, ~f : FnRef, ~args : Values)
  Apply(~ty : Type, ~f : Value, ~args : Values)
  ExtCall(~ty : Type, ~ext_f : String, ~args : Values)
  LoadSymbol(~ty : Type, ~sref : SymbolRef)
  Alloc(~ty : Type, ~mref : MemRef, ~fills : Fills)
  Store(~ty : Type, ~ma : Address, ~val : Value)
  Load(~ty : Type, ~ma : Address)
} derive(Show, Eq)

pub enum Fill {
  Zero
  Init(~val : Value)
  Field(~index : Int, ~val : Value)
} derive(Show, Eq)

// Mem is used to identify a memory region
pub enum Mem {
  Heap(~ty : Type, ~len : Value) // heap memory with given size. Value must be integer-compatible.
  Stack(~ty : Type, ~len : Value) // stack memory with given size. Value must be integer-compatible.
} derive(Show, Eq)

pub enum Width {
  WidthPtr
  Width64
  Width32
  Mix(~width : Width, ~other : Width)
} derive(Eq)

pub enum Address {
  Field(~ty : Type, ~val : Value, ~index : Int, ~width : Width) // field (constant offset) of memory. Value must be ptr-compatible.
  Offset(~ty : Type, ~val : Value, ~offset : Value, ~width : Width) // offset (variable offset) of memory. Value must be ptr-compatible.
} derive(Show, Eq)

pub enum Control {
  Jump(~target : BlockRef, ~args : Values)
  Return(~args : Values)
  Branch(
    ~cond : Value,
    ~t : BlockRef,
    ~t_args : Values,
    ~f : BlockRef,
    ~f_args : Values
  )
  Select(~cond : Value, ~targets : Array[BlockRef], ~args : Values)
  Panic
} derive(Show, Eq)

pub fn Block::all_basic_blocks(self : Block, func : Fn) -> Array[BasicBlock] {
  let result = []
  match self {
    BasicBlock(~bb) => result.push(bb)
    Loop(~l) => {
      result.append(func.blocks[l.header].unwrap().all_basic_blocks(func))
      for bref in l.blocks {
        result.append(func.blocks[bref].unwrap().all_basic_blocks(func))
      }
    }
  }
  result
}

pub fn Block::is_basic_block(self : Block) -> Bool {
  match self {
    BasicBlock(_) => true
    _ => false
  }
}

pub fn Block::bb_or_header_bb(self : Block, func : Fn) -> BasicBlock {
  match self {
    BasicBlock(~bb) => bb
    Loop(~l) => func.blocks[l.header].unwrap().bb_or_header_bb(func)
  }
}

pub fn Insn::ty(self : Insn) -> Type {
  match self {
    Op(~ty, ..)
    | Call(~ty, ..)
    | Apply(~ty, ..)
    | ExtCall(~ty, ..) | LoadSymbol(~ty, ..) | Alloc(~ty, ..) | Load(~ty, ..) =>
      ty
    Store(..) => Type::Unit
  }
}

pub fn Width::size_of_lp64(self : Width) -> Int {
  match self {
    WidthPtr => 8
    Width64 => 8
    Width32 => 4
    Mix(~width, ~other) =>
      @math.maximum(width.size_of_lp64(), other.size_of_lp64())
  }
}

pub fn Width::size_of_ilp32(self : Width) -> Int {
  match self {
    WidthPtr => 4
    Width64 => 8
    Width32 => 4
    Mix(~width, ~other) =>
      @math.maximum(width.size_of_ilp32(), other.size_of_ilp32())
  }
}

pub fn Type::width(self : Type) -> Width {
  match self {
    Unit => Width32
    Bool => Width32
    Int32 => Width32
    Int64 => Width64
    Double => Width64
    ClosureFn(..) => WidthPtr
    DirectFn(..) => WidthPtr
    Tuple(..) => WidthPtr
    Array(..) => WidthPtr
    Ptr => WidthPtr
  }
}

pub fn Type::elem_width(self : Type) -> Width? {
  let x = match self {
    Tuple(~fields) =>
      fields
      .map(fn(field) { field.width() })
      .rev_fold(init=Width32, fn(a, b) { Mix(width=b, other=a) })
    Array(~elem) => elem.width()
    ClosureFn(..) => Width64
    _ => return None
  }
  Some(x)
}

pub fn Type::size_of_lp64(self : Type) -> Int {
  match self {
    Unit => 0
    Bool => 4
    Int32 => 4
    Int64 => 8
    Double => 8
    ClosureFn(..) => 8
    DirectFn(..) => 8
    Tuple(..) => 8
    Array(..) => 8
    Ptr => 8
  }
}

pub fn Type::size_of_ilp32(self : Type) -> Int {
  match self {
    Unit => 0
    Bool => 4
    Int32 => 4
    Int64 => 8
    Double => 8
    ClosureFn(..) => 4
    DirectFn(..) => 4
    Tuple(..) => 4
    Array(..) => 4
    Ptr => 4
  }
}

pub fn Type::elem_size_of_lp64(self : Type) -> Int {
  match self {
    Array(~elem) => elem.size_of_lp64()
    Tuple(~fields) => {
      let mut size = 0
      for field in fields {
        size = @math.maximum(size, field.size_of_lp64())
      }
      size
    }
    ClosureFn(..) => 8
    DirectFn(..) => 8
    _ => self.size_of_lp64()
  }
}

pub fn Type::elem_size_of_ilp32(self : Type) -> Int {
  match self {
    Array(~elem) => elem.size_of_ilp32()
    Tuple(~fields) => {
      let mut size = 0
      for field in fields {
        size = @math.maximum(size, field.size_of_ilp32())
      }
      size
    }
    ClosureFn(..) => 8
    DirectFn(..) => 8
    _ => self.size_of_ilp32()
  }
}

pub fn Value::ty(self : Value, core : Core, func : Fn) -> Type!Failure {
  match self {
    Unit => Type::Unit
    Bool(_) => Type::Bool
    Int32(_) => Type::Int32
    Int64(_) => Type::Int64
    Double(_) => Type::Double
    Insn(~iref) => {
      let insn = match func.insns.get(iref) {
        Some(insn) => insn
        None =>
          fail!("insn \{Show::to_string(iref)} not found in \{func.insns}")
      }
      insn.ty()
    }
    FreeVar(~fvref) => func.fvars[fvref.id - 1]
    BlockParam(~bref, ~bpref) => {
      let block = match func.blocks.get(bref) {
        Some(block) => block
        _ => fail!("bb not found")
      }
      match block {
        BasicBlock(~bb) => bb.params[bpref.id]
        Loop(_) =>
          fail!("block parameters of loop not supported: \{bref}, \{bpref}")
      }
    }
    FnParam(~fref, ~fpref) => {
      let func = match core.funcs.get(fref) {
        Some(func) => func
        None => fail!("ty: fn_param \{fref} - func not found")
      }
      match func.ty {
        DirectFn(~params, ..) => params[fpref.id]
        ClosureFn(~params, ..) => params[fpref.id]
        _ => fail!("not a function type")
      }
    }
    Mem(~fref, ~mref) => {
      let func = match core.funcs.get(fref) {
        Some(func) => func
        None => fail!("ty: mem - func not found")
      }
      let mem = match func.mems.get(mref) {
        Some(mem) => mem
        None => fail!("mem not found")
      }
      match mem {
        Heap(~ty, ..) => ty
        Stack(~ty, ..) => ty
      }
    }
    Fn(~fref) => {
      let func = match core.funcs.get(fref) {
        Some(func) => func
        None => fail!("ty: fn \{fref} - func not found")
      }
      func.ty
    }
    Self(~fref) => {
      let func = match core.funcs.get(fref) {
        Some(func) => func
        None => fail!("ty: self \{fref} - func not found")
      }
      guard (match func.ty {
        ClosureFn(..) => true
        _ => false
      }) else {
        fail!("not a closure")
      }
      func.ty
    }
  }
}

pub fn Fn::next_block_id(self : Fn) -> Id {
  let mut id = -1
  for block in self.blocks {
    let (id_used, _) = block
    id = @math.maximum(id, id_used.id)
  }
  Id::new(id + 1)
}

pub fn Control::successors(self : Control) -> Array[BlockRef] {
  match self {
    Jump(~target, ..) => [target]
    Return(_) => []
    Branch(~t, ~f, ..) => [t, f]
    Select(~targets, ..) => targets
    Panic => []
  }
}

fn Op::from_knf(knf : @knf.Knf) -> Op? {
  match knf {
    Add(_, _) | FAdd(_, _) => Some(Op::Add)
    Sub(_, _) | FSub(_, _) => Some(Op::Sub)
    Mul(_, _) | FMul(_, _) => Some(Op::Mul)
    Div(_, _) | FDiv(_, _) => Some(Op::Div)
    Neg(_) | FNeg(_) => Some(Op::Neg)
    IfEq(_, _, _, _) => Some(Op::Eq)
    IfLe(_, _, _, _) => Some(Op::Le)
    _ => None
  }
}

pub fn Value::replace_all_uses_with(
  self : Value,
  func : Fn,
  block : BasicBlock,
  value : Value
) -> Unit {
  fn replace(v : Value) -> Value {
    if v == self {
      value
    } else {
      v
    }
  }

  fn replace_addr(addr : Address) -> Address {
    match addr {
      Field(~ty, ~val, ~index, ~width) =>
        Address::Field(~ty, val=replace(val), ~index, ~width)
      Offset(~ty, ~val, ~offset, ~width) =>
        Address::Offset(~ty, val=replace(val), offset=replace(offset), ~width)
    }
  }

  fn replace_fill(fill : Fill) -> Fill {
    match fill {
      Zero => Zero
      Init(~val) => Init(val=replace(val))
      Field(~index, ~val) => Field(~index, val=replace(val))
    }
  }

  // enumerate instructions
  let insn_updates = []
  let bb = func.blocks[block.id].unwrap().bb_or_header_bb(func)
  for iref in bb.seq {
    let insn = func.insns[iref].unwrap()
    // check if old value is a child of the instruction
    let new_insn = match insn {
      Op(~ty, ~op, ~args) => Op(~ty, ~op, args=args.map(replace))
      Call(~ty, ~f, ~args) => Call(~ty, ~f, args=args.map(replace))
      Apply(~ty, ~f, ~args) => Apply(~ty, ~f, args=args.map(replace))
      ExtCall(~ty, ~ext_f, ~args) =>
        ExtCall(~ty, ~ext_f, args=args.map(replace))
      LoadSymbol(..) => insn
      Alloc(~ty, ~mref, ~fills) =>
        Alloc(~ty, ~mref, fills=fills.map(replace_fill))
      Store(~ty, ~ma, ~val) => Store(~ty, ma=replace_addr(ma), val=replace(val))
      Load(~ty, ~ma) => Load(~ty, ma=replace_addr(ma))
    }
    insn_updates.push((iref, new_insn))
  }
  // enumerate controls
  let bb = bb
  bb.control = match bb.control {
    Jump(~target, ~args) => Jump(~target, args=args.map(replace))
    Return(~args) => Return(args=args.map(replace))
    Branch(~cond, ~t_args, ~f_args, ~t, ~f) =>
      Branch(
        cond=replace(cond),
        t_args=t_args.map(replace),
        f_args=f_args.map(replace),
        ~t,
        ~f,
      )
    Select(~cond, ~targets, ~args) =>
      Select(cond=replace(cond), ~targets, args=args.map(replace))
    Panic => Panic
  }
  // bulk update
  for insnpair in insn_updates {
    let (handle, new_insn) = insnpair
    func.insns.set(handle, new_insn)
  }
}

pub impl Hash for Value with hash_combine(self, hasher) {
  match self {
    Unit => hasher.combine_byte(b'0')
    Bool(~val) => {
      hasher.combine_int(1)
      hasher.combine(val)
    }
    Int32(~val) => {
      hasher.combine_int(2)
      hasher.combine(val)
    }
    Int64(~val) => {
      hasher.combine_int(3)
      hasher.combine(val)
    }
    Double(~val) => {
      hasher.combine_int(4)
      hasher.combine(val)
    }
    Insn(~iref) => {
      hasher.combine_int(5)
      hasher.combine(iref)
    }
    Mem(~fref, ~mref) => {
      hasher.combine_int(6)
      hasher.combine(fref)
      hasher.combine(mref)
    }
    Fn(~fref) => {
      hasher.combine_int(7)
      hasher.combine(fref)
    }
    FreeVar(~fvref, ..) => {
      hasher.combine_int(9)
      hasher.combine(fvref)
    }
    BlockParam(~bref, ~bpref) => {
      hasher.combine_int(10)
      hasher.combine(bref)
      hasher.combine(bpref)
    }
    FnParam(~fref, ~fpref) => {
      hasher.combine_int(11)
      hasher.combine(fref)
      hasher.combine(fpref)
    }
    Self(~fref) => {
      hasher.combine_int(12)
      hasher.combine(fref)
    }
  }
}

pub impl Show for RoundMode with output(self, logger) {
  match self {
    Rne => logger.write_string("rne")
    Rtz => logger.write_string("rtz")
    Rdn => logger.write_string("rdn")
    Rup => logger.write_string("rup")
    Rmm => logger.write_string("rmm")
  }
}

pub impl Show for Width with output(self, logger) {
  match self {
    WidthPtr => logger.write_string("@ptr")
    Width64 => logger.write_string("@64")
    Width32 => logger.write_string("@32")
    Mix(~width, ~other) => {
      logger.write_string("@(")
      logger.write_object(width)
      logger.write_string(", ")
      logger.write_object(other)
      logger.write_char(')')
    }
  }
}
