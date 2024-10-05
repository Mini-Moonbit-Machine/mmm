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
  id : LoopRef
  mut parent : LoopRef
  mut header : BlockRef // must be a basic block
  blocks : Array[BlockRef]
} derive(Show, Eq)

pub enum Block {
  BasicBlock(~bb : BasicBlock)
  Loop(~l : Loop)
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
  Op(~ty : Type, ~op : Op, ~args : Values)
  Call(~ty : Type, ~f : Value, ~args : Values)
  Apply(~ty : Type, ~f : Value, ~args : Values)
  ExtCall(~ty : Type, ~ext_f : String, ~args : Values)
  Alloc(~ty : Type, ~mref : MemRef, ~fills : Fills)
  Store(~ty : Type, ~ma : Address, ~val : Value)
  Load(~ty : Type, ~ma : Address)
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
