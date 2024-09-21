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

pub struct Core {
  names : MutMap[String, FnRef]
  funcs : MutMap[FnRef, Fn]
  globals : Array[(String, Value)]
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
  mut parent : LoopRef
  blocks : Array[BlockRef]
} derive(Show, Eq)

pub enum Block {
  BasicBlock(BasicBlock)
  Loop(Loop)
} derive(Show)

pub typealias Type = @closure.LowType

pub enum Value {
  Unit
  Bool(Bool)
  Int(Int)
  Double(Double)
  Insn(InsnRef)
  Mem(FnRef, MemRef)
  Fn(FnRef)
  GlobalThunk(FnRef)
  FreeVar(Type, FnFreeVarRef)
  BlockParam(BlockRef, BlockParamRef)
  FnParam(FnRef, FnParamRef)
  Self(FnRef)
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
  Op(Type, Op, Array[Value])
  Call(Type, Value, Array[Value])
  Apply(Type, Value, Array[Value])
  ExtCall(Type, String, Array[Value])
  Alloc(Type, MemRef, Array[Fill])
  Store(Type, MemAccess, Value)
  Load(Type, MemAccess)
} derive(Show, Eq)

pub enum Fill {
  Zero
  Init(Value)
  Field(Int, Value)
  Offset(Value, Value)
} derive(Show, Eq)

// Mem is used to identify a memory region
pub enum Mem {
  Heap(Type, Value) // heap memory with given size. Value must be integer-compatible.
  Stack(Type, Value) // stack memory with given size. Value must be integer-compatible.
} derive(Show, Eq)

pub enum MemAccess {
  Field(Type, Value, Int) // field (constant offset) of memory. Value must be ptr-compatible.
  Offset(Type, Value, Value) // offset (variable offset) of memory. Value must be ptr-compatible.
} derive(Show, Eq)

pub enum Control {
  Jump(BlockRef, Array[Value])
  Return(Array[Value])
  Branch(Value, BlockRef, Array[Value], BlockRef, Array[Value])
  Select(Value, Array[BlockRef], Array[Value])
  Panic
} derive(Show, Eq)

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
