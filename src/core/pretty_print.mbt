pub struct PrettyCore {
  core : Core
}

pub fn Core::to_pretty_print(self : Core) -> PrettyCore {
  { core: self }
}

pub struct PrettyFn {
  func : Fn
}

pub fn Fn::to_pretty_print(self : Fn) -> PrettyFn {
  { func: self }
}

fn print_op(logger : @util.IndentLogger, op : Op) -> Unit {
  match op {
    Add => logger.write_string("add")
    Sub => logger.write_string("sub")
    Mul => logger.write_string("mul")
    Div => logger.write_string("div")
    Eq => logger.write_string("eq")
    Le => logger.write_string("le")
    Lnot => logger.write_string("lnot")
    Neg => logger.write_string("neg")
    D2I(~rm) => logger.write_string("conv.d2i.\{Show::to_string(rm)}")
    I2D => logger.write_string("conv.i2d")
    Abs => logger.write_string("abs")
    Sqrt => logger.write_string("sqrt")
  }
}

fn print_type(logger : @util.IndentLogger, ty : Type) -> Unit {
  match ty {
    Unit => logger.write_string("unit")
    Bool => logger.write_string("bool")
    Int32 => logger.write_string("int32")
    Int64 => logger.write_string("int64")
    Double => logger.write_string("double")
    ClosureFn(~params, ~ty) => {
      logger.write_string("closure(")
      print_types(logger, params)
      logger.write_string(") -> ")
      print_type(logger, ty)
    }
    DirectFn(~params, ~ty) => {
      logger.write_string("fn(")
      print_types(logger, params)
      logger.write_string(") -> ")
      print_type(logger, ty)
    }
    UnboxedTuple(~fields) => {
      logger.write_string("#(")
      print_types(logger, fields)
      logger.write_string(")")
    }
    Tuple(~fields) => {
      logger.write_string("(")
      print_types(logger, fields)
      logger.write_string(")")
    }
    Array(~elem) => {
      logger.write_string("array[")
      print_type(logger, elem)
      logger.write_string("]")
    }
    Ptr => logger.write_string("ptr")
  }
}

fn print_value(logger : @util.IndentLogger, val : Value) -> Unit {
  match val {
    Unit => logger.write_string("()")
    Bool(~val) => logger.write_string("\{val}")
    Int32(~val) => logger.write_string("\{val}")
    Int64(~val) => logger.write_string("\{val}")
    Double(~val) => logger.write_string("\{val}")
    Insn(~iref) => logger.write_string("\{iref}")
    BlockParam(bref=BlockRef({ id }), bpref=BlockParamRef({ id: id2 })) =>
      logger.write_string("$b\{id}.\{id2}")
    FnParam(fpref=FnParamRef({ id }), ..) => logger.write_string("$f\{id}")
    FreeVar(~fvref, ..) => logger.write_string("\{fvref}")
    Mem(fref=FnRef({ id }), mref=MemRef({ id: id2 })) =>
      logger.write_string("f\{id}.m\{id2}")
    Fn(~fref) => logger.write_string("\{fref}")
    Self(_) => logger.write_string("self")
  }
}

fn print_types(logger : @util.IndentLogger, types : Array[Type]) -> Unit {
  let mut first = true
  for ty in types {
    if first {
      first = false
    } else {
      logger.write_string(", ")
    }
    print_type(logger, ty)
  }
}

fn print_values(logger : @util.IndentLogger, values : Array[Value]) -> Unit {
  let mut first = true
  for val in values {
    if first {
      first = false
    } else {
      logger.write_string(", ")
    }
    print_value(logger, val)
  }
}

fn print_mem_ref(logger : @util.IndentLogger, func : Fn, mr : MemRef) -> Unit {
  let mem = func.mems.get(mr).unwrap()
  logger.write_string("<\{mr}: ")
  match mem {
    Heap(~ty, ~len) => {
      print_type(logger, ty)
      logger.write_string(" heap(")
      print_value(logger, len)
      logger.write_string(")")
    }
    Stack(~ty, ~len) => {
      print_type(logger, ty)
      logger.write_string(" stack(")
      print_value(logger, len)
      logger.write_string(")")
    }
  }
  logger.write_string(">")
}

fn print_mem_access(
  logger : @util.IndentLogger,
  _func : Fn,
  ma : Address
) -> Unit {
  match ma {
    Field(~ty, ~val, ~index, ~width) => {
      print_type(logger, ty)
      logger.write_string(" field(")
      print_value(logger, val)
      logger.write_string(", \{index} * \{Show::to_string(width)})")
    }
    Offset(~ty, ~val, ~offset, ~width) => {
      print_type(logger, ty)
      logger.write_string(" offset(")
      print_value(logger, val)
      logger.write_string(", ")
      print_value(logger, offset)
      logger.write_string(" * \{Show::to_string(width)})")
    }
    Raw(~ty, ~val, ~raw_offset) => {
      print_type(logger, ty)
      logger.write_string(" raw(")
      print_value(logger, val)
      logger.write_string(", ")
      print_value(logger, raw_offset)
      logger.write_string(")")
    }
  }
}

fn print_fill(logger : @util.IndentLogger, fill : Fill) -> Unit {
  match fill {
    Zero => logger.write_string("[..] = 0")
    Init(~val) => {
      logger.write_string("[..] = ")
      print_value(logger, val)
    }
    Field(~index, ~val) => {
      logger.write_string("[\{index}] = ")
      print_value(logger, val)
    }
  }
}

fn print_fills(logger : @util.IndentLogger, fills : Array[Fill]) -> Unit {
  let mut first = true
  for fill in fills {
    if first {
      first = false
    } else {
      logger.write_string(", ")
    }
    print_fill(logger, fill)
  }
}

fn print_control(logger : @util.IndentLogger, control : Control) -> Unit {
  match control {
    Jump(~target, ~args) => {
      logger.write_string("jump \{target}(")
      print_values(logger, args)
      logger.write_string(")\n")
    }
    Return(~args) => {
      logger.write_string("return ")
      print_values(logger, args)
      logger.write_string("\n")
    }
    Branch(~cond, ~t, ~t_args, ~f, ~f_args) => {
      logger.write_string("br ")
      print_value(logger, cond)
      logger.write_string(" \{t}(")
      print_values(logger, t_args)
      logger.write_string(") \{f}(")
      print_values(logger, f_args)
      logger.write_string(")\n")
    }
    Select(~cond, ~targets, ~fallback) => {
      logger.write_string("select ")
      print_value(logger, cond)
      logger.write_string(" [")
      let mut first = true
      for target in targets {
        if first {
          first = false
        } else {
          logger.write_string(", ")
        }
        logger.write_string("\{target}")
      }
      logger.write_string("] else \{fallback}\n")
    }
    Panic => logger.write_string("panic\n")
  }
}

fn print_insn(
  logger : @util.IndentLogger,
  func : Fn,
  ir : InsnRef,
  insn : Insn
) -> Unit {
  match insn {
    Op(~ty, ~op, ~args) => {
      logger.write_string("let \{ir}: ")
      print_type(logger, ty)
      logger.write_string(" = ")
      print_op(logger, op)
      logger.write_string("(")
      print_values(logger, args)
      logger.write_string(")\n")
    }
    Call(~ty, ~f, ~args) => {
      logger.write_string("let \{ir}: ")
      print_type(logger, ty)
      logger.write_string(" = call \{f}(")
      print_values(logger, args)
      logger.write_string(")\n")
    }
    Apply(~ty, ~f, ~args) => {
      logger.write_string("let \{ir}: ")
      print_type(logger, ty)
      logger.write_string(" = apply ")
      print_value(logger, f)
      logger.write_string("(")
      print_values(logger, args)
      logger.write_string(")\n")
    }
    ExtCall(~ty, ~ext_f, ~args) => {
      logger.write_string("let \{ir}: ")
      print_type(logger, ty)
      logger.write_string(" = ext_call \{ext_f}(")
      print_values(logger, args)
      logger.write_string(")\n")
    }
    LoadSymbol(~ty, sref=sym) => {
      logger.write_string("let \{ir}: ")
      print_type(logger, ty)
      logger.write_string(" = load_symbol \{sym}\n")
    }
    Pack(~ty, ~vals) => {
      logger.write_string("let \{ir}: ")
      print_type(logger, ty)
      logger.write_string(" = pack ")
      print_values(logger, vals)
      logger.write_string("\n")
    }
    Extract(~ty, ~val, ~index) => {
      logger.write_string("let \{ir}: ")
      print_type(logger, ty)
      logger.write_string(" = extract ")
      print_value(logger, val)
      logger.write_string(" \{index}\n")
    }
    Alloc(~mref, ~fills, ..) => {
      logger.write_string("let _ = alloc ")
      print_mem_ref(logger, func, mref)
      logger.write_string(" <- ")
      print_fills(logger, fills)
      logger.write_string("\n")
    }
    Store(~ma, ~val, ..) => {
      logger.write_string("let _ = store ")
      print_mem_access(logger, func, ma)
      logger.write_string(" <- ")
      print_value(logger, val)
      logger.write_string("\n")
    }
    Load(~ty, ~ma) => {
      logger.write_string("let \{ir}: ")
      print_type(logger, ty)
      logger.write_string(" = load ")
      print_mem_access(logger, func, ma)
      logger.write_string("\n")
    }
    ComputeAddress(~ma) => {
      logger.write_string("let \{ir}: ptr")
      logger.write_string(" = compute_address ")
      print_mem_access(logger, func, ma)
      logger.write_string("\n")
    }
  }
}

fn print_block_short(logger : @util.IndentLogger, bref : BlockRef) -> Unit {
  logger.write_string(".block #\{bref}")
}

fn print_bb(logger : @util.IndentLogger, func : Fn, bb : BasicBlock) -> Unit {
  logger.write_string(".bb #\{bb.id} (")
  print_types(logger, bb.params)
  logger.write_string(") {\n")
  logger.indent()
  for ir in bb.seq {
    let insn = func.insns.get(ir).unwrap()
    print_insn(logger, func, ir, insn)
  }
  print_control(logger, bb.control)
  logger.outdent()
  logger.write_string("}\n")
}

fn print_loop(logger : @util.IndentLogger, _func : Fn, lp : Loop) -> Unit {
  logger.write_string(
    ".loop #\{lp.id} pre #\{lp.pre_header} header #\{lp.header} {\n",
  )
  logger.indent()
  for block in lp.blocks {
    print_block_short(logger, block)
    logger.write_string("\n")
  }
  logger.outdent()
  logger.write_string("}\n")
}

fn print_block(logger : @util.IndentLogger, func : Fn, block : Block) -> Unit {
  match block {
    BasicBlock(~bb) => print_bb(logger, func, bb)
    Loop(~l) => print_loop(logger, func, l)
  }
}

fn print_func(logger : @util.IndentLogger, val : Fn) -> Unit {
  logger.write_string(".fn \{val.name} #\{val.id} : ")
  print_type(logger, val.ty)
  logger.write_string("\n.freevars (")
  print_types(logger, val.fvars)
  logger.write_string(")\n.entry \{val.entry} {\n")
  logger.indent()
  let all_blocks = val.blocks.iter().to_array()
  all_blocks.sort_by(fn(b1, b2) { b1.0._.id.compare(b2.0._.id) })
  for block in all_blocks {
    let (_, block) = block
    print_block(logger, val, block)
  }
  logger.outdent()
  logger.write_string("}\n")
}

fn print(logger : @util.IndentLogger, val : Core) -> Unit {
  logger.write_string(
    #|# name conventions used in the core ir:
    #|# - functions: f<id>
    #|# - blocks: b<id>
    #|# - instructions: %<id>
    #|# - memory: m<id>
    #|# - free variables: ^<id>
    #|# - function parameters: $f<pid>
    #|# - block parameters: $b<bid>.<pid>
    #|.core
    #|
    ,
  )
  for func in val.names {
    let (_, fref) = func
    let func = val.funcs.get(fref).unwrap()
    print_func(logger, func)
  }
  for thunk in val.global_names {
    let (thunk_name, sym) = thunk
    logger.write_string(".thunk \{sym} \{thunk_name}\n")
  }
}

pub fn output(self : PrettyCore, logger : Logger) -> Unit {
  let indent_logger = @util.IndentLogger::new(logger)
  print(indent_logger, self.core)
}

pub fn to_string(self : PrettyCore) -> String {
  Show::to_string(self)
}

pub fn output(self : PrettyFn, logger : Logger) -> Unit {
  let indent_logger = @util.IndentLogger::new(logger)
  print_func(indent_logger, self.func)
}

pub fn to_string(self : PrettyFn) -> String {
  Show::to_string(self)
}
