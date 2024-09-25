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
  BasicBlock(~bb : BasicBlock)
  Loop(~l : Loop)
} derive(Show)

pub enum Type {
  Unit
  Bool
  Int
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
  Int(~val : Int)
  Double(~val : Double)
  Insn(~iref : InsnRef)
  Mem(~fref : FnRef, ~mref : MemRef)
  Fn(~fref : FnRef)
  GlobalThunk(~fref : FnRef)
  FreeVar(~ty : Type, ~fvref : FnFreeVarRef)
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
  Op(~ty : Type, ~op : Op, ~args : Array[Value])
  Call(~ty : Type, ~f : Value, ~args : Array[Value])
  Apply(~ty : Type, ~f : Value, ~args : Array[Value])
  ExtCall(~ty : Type, ~ext_f : String, ~args : Array[Value])
  Alloc(~ty : Type, ~mref : MemRef, ~fills : Array[Fill])
  Store(~ty : Type, ~ma : MemAccess, ~val : Value)
  Load(~ty : Type, ~ma : MemAccess)
} derive(Show, Eq)

pub enum Fill {
  Zero
  Init(~val : Value)
  Field(~index : Int, ~val : Value)
  Offset(~offset : Value, ~val : Value)
} derive(Show, Eq)

// Mem is used to identify a memory region
pub enum Mem {
  Heap(~ty : Type, ~len : Value) // heap memory with given size. Value must be integer-compatible.
  Stack(~ty : Type, ~len : Value) // stack memory with given size. Value must be integer-compatible.
} derive(Show, Eq)

pub enum MemAccess {
  Field(~ty : Type, ~val : Value, ~index : Int) // field (constant offset) of memory. Value must be ptr-compatible.
  Offset(~ty : Type, ~val : Value, ~offset : Value) // offset (variable offset) of memory. Value must be ptr-compatible.
} derive(Show, Eq)

pub enum Control {
  Jump(~target : BlockRef, ~args : Array[Value])
  Return(~args : Array[Value])
  Branch(
    ~cond : Value,
    ~t : BlockRef,
    ~t_args : Array[Value],
    ~f : BlockRef,
    ~f_args : Array[Value]
  )
  Select(~cond : Value, ~targets : Array[BlockRef], ~args : Array[Value])
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
