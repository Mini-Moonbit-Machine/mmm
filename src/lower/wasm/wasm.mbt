pub type Label String derive(Eq, Hash)

pub type Name String derive(Eq, Hash)

pub type TableIndex Int derive(Eq, Hash)

pub type LabelIndex Int derive(Eq, Hash)

pub fn output(self : Label, logger : Logger) -> Unit {
  logger.write_string("$\{self._}")
}

pub fn to_string(self : Label) -> String {
  Show::to_string(self)
}

pub fn output(self : Name, logger : Logger) -> Unit {
  logger.write_string("$\{self._}")
}

pub fn to_string(self : Name) -> String {
  Show::to_string(self)
}

pub fn output(self : TableIndex, logger : Logger) -> Unit {
  logger.write_string("\{self._}")
}

pub fn to_string(self : TableIndex) -> String {
  Show::to_string(self)
}

pub fn output(self : LabelIndex, logger : Logger) -> Unit {
  logger.write_string("\{self._}")
}

pub fn to_string(self : LabelIndex) -> String {
  Show::to_string(self)
}

pub enum Type {
  I32
  F64
  FunRef
}

pub fn Type::output(self : Type, logger : Logger) -> Unit {
  match self {
    I32 => logger.write_string("i32")
    F64 => logger.write_string("f64")
    FunRef => logger.write_string("funcref")
  }
}

pub fn to_string(self : Type) -> String {
  Show::to_string(self)
}

pub enum WasmInsn {
  // variable
  GetLocal(~name : Name)
  SetLocal(~name : Name)
  GetGlobal(~name : Name)
  SetGlobal(~name : Name)
  LocalTee
  Drop
  // arithmetic
  I32Add
  I32Sub
  I32Mul
  I32Div
  I32Xor
  I32Or
  I32And
  I32Sll
  I32Srl
  I32Sra
  I32Eq
  I32Le
  I32Eqz
  // memory access
  I32Store
  I32Load
  F64Store
  F64Load
  // control flow
  Block
  If
  Else
  End
  Br(~target : Label)
  BrTable(~targets : Array[LabelIndex])
  Loop(~label : Label)
  Call(~target : Label)
  Return
  CallIndirect(~typename : Label)
  // float
  F64Add
  F64Sub
  F64Mul
  F64Div
  F64Eq
  F64Le
  F64Neg
  F64Abs
  F64Sqrt
  // conversions
  F64CvtI32
  I32TruncF64
  I32WrapI64
  I64ExtendI32
  // const
  I32Const(~val : Int)
  F64Const(~val : Double)
  // tables
  TableSet(~label : Label)
  // reference
  RefFunc(~label : Label)
  // comment
  Comment(~comment : String)
}

pub struct MTable {
  name : Label
  size : Int
  elems : Array[Label]
}

pub struct MFn {
  name : Label
  args : Array[(Name, Type)]
  ret : Type?
  locals : Array[(Name, Type)]
  seq : Array[WasmInsn]
}

pub struct MCore {
  func_type_ctx : MutMap[Label, (Array[Type], Type?)]
  table : MTable
  globals : MutMap[Name, Type]
  mfuncs : Array[MFn]
}

pub fn MFn::output(self : MFn, logger : Logger) -> Unit {
  logger.write_string("(func \{self.name}\n")
  for arg in self.args {
    let (name, ty) = arg
    logger.write_string("  (param \{name} \{ty})\n")
  }
  match self.ret {
    None => ()
    Some(ty) => logger.write_string("  (result \{ty})\n")
  }
  for local in self.locals {
    let (name, ty) = local
    logger.write_string("  (local \{name} \{ty})\n")
  }
  for insn in self.seq {
    logger.write_string("\{insn}\n")
  }
  logger.write_string(")\n")
}

pub fn MCore::output(self : MCore, logger : Logger) -> Unit {
  let imports =
    #|(memory (import "moonbit" "memory") 10)
    #|(import "moonbit" "minimbt_read_int" (func $minimbt_read_int (result i32)))
    #|(import "moonbit" "minimbt_print_int" (func $minimbt_print_int (param i32)))
    #|(import "moonbit" "minimbt_read_char" (func $minimbt_read_char (result i32)))
    #|(import "moonbit" "minimbt_print_char" (func $minimbt_print_char (param i32)))
    #|(import "moonbit" "minimbt_print_endline" (func $minimbt_print_endline))
    #|(import "moonbit" "minimbt_int_of_float" (func $minimbt_int_of_float (param f64) (result i32)))
    #|(import "moonbit" "minimbt_float_of_int" (func $minimbt_float_of_int (param i32) (result f64)))
    #|(import "moonbit" "minimbt_truncate" (func $minimbt_truncate(param f64) (result i32)))
    #|(import "moonbit" "minimbt_floor" (func $minimbt_floor (param f64) (result f64)))
    #|(import "moonbit" "minimbt_abs_float" (func $minimbt_abs_float (param f64) (result f64)))
    #|(import "moonbit" "minimbt_sqrt" (func $minimbt_sqrt (param f64) (result f64)))
    #|(import "moonbit" "minimbt_sin" (func $minimbt_sin (param f64) (result f64)))
    #|(import "moonbit" "minimbt_cos" (func $minimbt_cos (param f64) (result f64)))
    #|(import "moonbit" "minimbt_atan" (func $minimbt_atan (param f64) (result f64)))
    #|(import "moonbit" "minimbt_malloc" (func $minimbt_malloc (param i32) (result i32)))
    #|(import "moonbit" "minimbt_create_array" (func $minimbt_create_array (param i32) (param i32) (result i32)))
    #|(import "moonbit" "minimbt_create_float_array" (func $minimbt_create_float_array (param i32) (param f64) (result i32)))
    #|(import "moonbit" "minimbt_create_ptr_array" (func $minimbt_create_ptr_array (param i32) (param i32) (result i32)))
    #|
  let elems = self.table.elems
    .map(fn(label) { "(ref.func \{label})" })
    .join(" ")
  let table = "(table \{self.table.name} \{self.table.size} funcref)\n" +
    "(elem (table \{self.table.name}) (offset i32.const 0) funcref \{elems})\n"
  let types = self.func_type_ctx
    .iter()
    .map(
      fn(ntypair) {
        let (label, (params, ret)) = ntypair
        let mut func_ty = "func"
        for param in params {
          func_ty = func_ty + " (param \{param})"
        }
        match ret {
          None => ()
          Some(ty) => func_ty = func_ty + " (result \{ty})"
        }
        "(type \{label} (\{func_ty}))"
      },
    )
    .collect()
  let globals = self.globals
    .iter()
    .map(
      fn(ntypair) {
        "(global \{ntypair.0} (mut \{ntypair.1}) (\{ntypair.1}.const 0))"
      },
    )
    .collect()
  logger.write_string(imports)
  logger.write_string(table)
  logger.write_string(types.join("\n"))
  logger.write_string("\n")
  logger.write_string(globals.join("\n"))
  logger.write_string("\n")
  for func in self.mfuncs {
    func.output(logger)
    logger.write_string("(export \"\{func.name}\" (func \{func.name}))\n")
  }
  let start_fn = Label("minimbt_main")
  logger.write_string("(start \{start_fn})\n")
}

pub fn to_string(self : MCore) -> String {
  Show::to_string(self)
}

pub fn WasmInsn::output(self : WasmInsn, logger : Logger) -> Unit {
  logger.write_string("  ")
  match self {
    GetLocal(~name) => logger.write_string("local.get \{name}")
    SetLocal(~name) => logger.write_string("local.set \{name}")
    GetGlobal(~name) => logger.write_string("global.get \{name}")
    SetGlobal(~name) => logger.write_string("global.set \{name}")
    LocalTee => logger.write_string("local.tee")
    Drop => logger.write_string("drop")
    I32Add => logger.write_string("i32.add")
    I32Sub => logger.write_string("i32.sub")
    I32Mul => logger.write_string("i32.mul")
    I32Div => logger.write_string("i32.div")
    I32Xor => logger.write_string("i32.xor")
    I32Or => logger.write_string("i32.or")
    I32And => logger.write_string("i32.and")
    I32Sll => logger.write_string("i32.shl")
    I32Srl => logger.write_string("i32.shr_u")
    I32Sra => logger.write_string("i32.shr_s")
    I32Eq => logger.write_string("i32.eq")
    I32Le => logger.write_string("i32.le_s")
    I32Eqz => logger.write_string("i32.eqz")
    I32Store => logger.write_string("i32.store")
    I32Load => logger.write_string("i32.load")
    F64Store => logger.write_string("f64.store")
    F64Load => logger.write_string("f64.load")
    Block => logger.write_string("block")
    If => logger.write_string("if")
    Else => logger.write_string("else")
    End => logger.write_string("end")
    Br(~target) => logger.write_string("br \{target}")
    BrTable(~targets) => {
      let index = targets.map(fn(target) { target.to_string() }).join(" ")
      logger.write_string("br_table \{index}")
    }
    Loop(~label) => logger.write_string("loop \{label}")
    Call(~target) => logger.write_string("call \{target}")
    Return => logger.write_string("return")
    CallIndirect(~typename) =>
      logger.write_string("call_indirect (type \{typename})")
    F64Add => logger.write_string("f64.add")
    F64Sub => logger.write_string("f64.sub")
    F64Mul => logger.write_string("f64.mul")
    F64Div => logger.write_string("f64.div")
    F64Eq => logger.write_string("f64.eq")
    F64Le => logger.write_string("f64.le")
    F64Neg => logger.write_string("f64.neg")
    F64Abs => logger.write_string("f64.abs")
    F64Sqrt => logger.write_string("f64.sqrt")
    F64CvtI32 => logger.write_string("f64.convert_i32_s")
    I32TruncF64 => logger.write_string("i32.trunc_f64_s")
    I32WrapI64 => logger.write_string("i32.wrap_i64")
    I64ExtendI32 => logger.write_string("i64.extend_i32_s")
    I32Const(~val) => logger.write_string("i32.const \{val}")
    F64Const(~val) => logger.write_string("f64.const \{val}")
    TableSet(~label) => logger.write_string("table.set \{label}")
    RefFunc(~label) => logger.write_string("ref.func \{label}")
    Comment(~comment) => logger.write_string(";; \{comment}")
  }
}

pub fn to_string(self : WasmInsn) -> String {
  Show::to_string(self)
}
