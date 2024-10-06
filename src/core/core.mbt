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

pub type LoopRef Id derive(Eq, Hash)

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

pub fn output(self : LoopRef, logger : Logger) -> Unit {
  logger.write_char('l')
  logger.write_object(self.id)
}

pub fn to_string(self : LoopRef) -> String {
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
  entry : BlockRef
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
  id : LoopRef
  mut parent : LoopRef
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

pub enum Op {
  Add
  Sub
  Mul
  Div
  Neg
  Lnot
  Eq
  Le
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

pub enum Address {
  Field(~ty : Type, ~val : Value, ~index : Int) // field (constant offset) of memory. Value must be ptr-compatible.
  Offset(~ty : Type, ~val : Value, ~offset : Value) // offset (variable offset) of memory. Value must be ptr-compatible.
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

pub fn Block::basic_block(self : Block, func : Fn) -> BasicBlock {
  match self {
    BasicBlock(~bb) => bb
    Loop(~l) => func.blocks[l.header].unwrap().basic_block(func)
  }
}

pub fn Insn::ty(self : Insn) -> Type {
  match self {
    Op(~ty, ..)
    | Call(~ty, ..)
    | Apply(~ty, ..)
    | ExtCall(~ty, ..)
    | LoadSymbol(~ty, ..) | Alloc(~ty, ..) | Store(~ty, ..) | Load(~ty, ..) =>
      ty
  }
}

pub fn Type::size(self : Type) -> Int {
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

pub fn Type::elem_size(self : Type) -> Int {
  match self {
    Array(~elem) => elem.size()
    ClosureFn(..) | Tuple(..) => 8
    _ => {
      println("Type::elem_size: not an array, tuple, or closure")
      0
    }
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
      match insn {
        Op(~ty, ..) => ty
        Apply(~ty, ..) => ty
        Call(~ty, ..) => ty
        ExtCall(~ty, ..) => ty
        LoadSymbol(~ty, ..) => ty
        Alloc(~ty, ..) => ty
        Load(~ty, ..) => ty
        Store(..) => Type::Unit
      }
    }
    FreeVar(~fvref) => func.fvars[fvref.id - 1]
    BlockParam(~bref, ~bpref) => {
      let block = match func.blocks.get(bref) {
        Some(block) => block
        _ => fail!("bb not found")
      }
      match block {
        BasicBlock(~bb) => bb.params[bpref.id]
        Loop(_) => fail!("loop not supported")
      }
    }
    FnParam(~fref, ~fpref) => {
      let func = match core.funcs.get(fref) {
        Some(func) => func
        None => fail!("func not found")
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
        None => fail!("func not found")
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
        None => fail!("func not found")
      }
      func.ty
    }
    Self(~fref) => {
      let func = match core.funcs.get(fref) {
        Some(func) => func
        None => fail!("func not found")
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
