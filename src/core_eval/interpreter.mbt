pub struct CoreInterpreter {}

pub fn CoreInterpreter::new() -> CoreInterpreter {
  CoreInterpreter::{  }
}

let trace = false

typealias Fn = @core.Fn

typealias FnRef = @core.FnRef

typealias MemRef = @core.MemRef

typealias InsnRef = @core.InsnRef

typealias BlockParamRef = @core.BlockParamRef

typealias BlockRef = @core.BlockRef

typealias FnParamRef = @core.FnParamRef

typealias FnFreeVarRef = @core.FnFreeVarRef

typealias Value = @core.Value

typealias MutMap[K, V] = @core.MutMap[K, V]

typealias Map[K, V] = @immut/hashmap.T[K, V]

typealias Block = @core.Block

typealias BasicBlock = @core.BasicBlock

typealias Insn = @core.Insn

typealias Control = @core.Control

typealias Fill = @core.Fill

enum Object {
  Uninit
  Unit
  Int32(Int)
  Int64(Int64)
  Bool(Bool)
  Double(Double)
  FnPtr(FnRef)
  Mem(mut ~fields : Array[Object])
} derive(Show)

pub struct Ctx {
  heap : MutMap[(FnRef, MemRef), Object]
  stack : Map[MemRef, Object]
  global : Map[@core.SymbolRef, Object]
  funcs : MutMap[FnRef, Fn]
  insns : Map[InsnRef, Object]
  blkargs : Map[BlockRef, Array[Object]]
  fnargs : Array[Object]
  closure : Object
} derive(Show)

fn Ctx::new(funcs : MutMap[FnRef, Fn]) -> Ctx {
  Ctx::{
    heap: MutMap::new(),
    stack: Map::new(),
    global: Map::new(),
    funcs,
    insns: Map::new(),
    blkargs: Map::new(),
    fnargs: Array::new(),
    closure: Unit,
  }
}

fn Ctx::set_block_args(
  self : Ctx,
  blkref : BlockRef,
  args : Array[Object]
) -> Ctx {
  {
    blkargs: self.blkargs.add(blkref, args),
    heap: self.heap,
    stack: self.stack,
    global: self.global,
    funcs: self.funcs,
    insns: self.insns,
    fnargs: self.fnargs,
    closure: self.closure,
  }
}

fn Ctx::set_function_args(self : Ctx, args : Array[Object]) -> Ctx {
  {
    fnargs: args,
    heap: self.heap,
    stack: self.stack,
    global: self.global,
    funcs: self.funcs,
    insns: self.insns,
    blkargs: self.blkargs,
    closure: self.closure,
  }
}

fn Ctx::set_closure(self : Ctx, closure : Object) -> Ctx {
  {
    closure,
    heap: self.heap,
    stack: self.stack,
    global: self.global,
    funcs: self.funcs,
    insns: self.insns,
    blkargs: self.blkargs,
    fnargs: self.fnargs,
  }
}

fn Ctx::update_insn(self : Ctx, ir : InsnRef, obj : Object) -> Ctx {
  {
    insns: self.insns.add(ir, obj),
    heap: self.heap,
    stack: self.stack,
    global: self.global,
    funcs: self.funcs,
    blkargs: self.blkargs,
    fnargs: self.fnargs,
    closure: self.closure,
  }
}

fn Ctx::update_heap(
  self : Ctx,
  func : FnRef,
  mem : MemRef,
  obj : Object
) -> Ctx {
  self.heap.set((func, mem), obj)
  self
}

fn Ctx::update_stack(self : Ctx, mem : MemRef, obj : Object) -> Ctx {
  {
    heap: self.heap,
    stack: self.stack.add(mem, obj),
    global: self.global,
    funcs: self.funcs,
    insns: self.insns,
    blkargs: self.blkargs,
    fnargs: self.fnargs,
    closure: self.closure,
  }
}

fn Ctx::update_global(self : Ctx, sym : @core.SymbolRef, obj : Object) -> Ctx {
  {
    global: self.global.add(sym, obj),
    heap: self.heap,
    stack: self.stack,
    funcs: self.funcs,
    insns: self.insns,
    blkargs: self.blkargs,
    fnargs: self.fnargs,
    closure: self.closure,
  }
}

pub fn CoreInterpreter::interpret(
  self : CoreInterpreter,
  core : @core.Core
) -> Unit!Failure {
  let { funcs, global_names: globals, .. } = core
  let mut ctx = Ctx::new(funcs)
  for thunk in globals {
    let (name, sref) = thunk
    let symbol = core.global_symbols[sref].unwrap()
    let func = match funcs.get(symbol.thunk) {
      Some(func) => func
      None => return fail!("function not found: \{name}")
    }
    let obj = self.eval_fn!(ctx, func)
    ctx = ctx.update_global(sref, obj)
  }
}

fn CoreInterpreter::eval_fn(
  self : CoreInterpreter,
  ctx : Ctx,
  func : Fn
) -> Object!Failure {
  let block = match func.blocks.get(func.entry) {
    Some(block) => block
    None => return fail!("entry block not found")
  }
  self.eval_block!(ctx, func, block)
}

fn CoreInterpreter::eval_block(
  self : CoreInterpreter,
  ctx : Ctx,
  func : Fn,
  block : Block
) -> Object!Failure {
  match block {
    BasicBlock(~bb) => self.eval_bb!(ctx, func, bb)
    Loop(_) => fail!("Not implemented")
  }
}

fn CoreInterpreter::eval_bb(
  self : CoreInterpreter,
  ctx : Ctx,
  func : Fn,
  bb : BasicBlock
) -> Object!Failure {
  let mut ctx = ctx
  for ir in bb.seq {
    let insn = match func.insns.get(ir) {
      Some(insn) => insn
      None => return fail!("insn not found: \{ir}")
    }
    if trace {
      println(":::: \{func.name} \{ir} = \{insn}")
    }
    let (ctx2, obj) = self.eval_insn!(ctx, func, insn)
    if trace {
      println(":::: \{func.name} \{ir} |-> \{obj}")
    }
    ctx = ctx2.update_insn(ir, obj)
  }
  self.eval_control!(ctx, func, bb.control)
}

fn CoreInterpreter::eval_fills(
  ctx : Ctx,
  fills : Array[Fill],
  obj : Object
) -> Unit!Failure {
  match obj {
    Mem(~fields) =>
      for fill in fills {
        match fill {
          Zero => fields.map_inplace(fn(_i) { Int32(0) })
          Init(~val) => {
            let v = CoreInterpreter::eval_value!(ctx, val)
            fields.map_inplace(fn(_i) { v })
          }
          Field(~index, ~val) => {
            let v = CoreInterpreter::eval_value!(ctx, val)
            if index < fields.length() {
              fields[index] = v
            } else {
              fail!(
                "field index out of bounds: \{index} out of \{fields.length()}",
              )
            }
            fields[index] = v
          }
        }
      }
    _ => fail!("not a mem object")
  }
}

fn CoreInterpreter::eval_insn(
  self : CoreInterpreter,
  ctx : Ctx,
  func : Fn,
  insn : Insn
) -> (Ctx, Object)!Failure {
  match insn {
    Op(~op, ~args, ..) => {
      let args = CoreInterpreter::eval_values!(ctx, args)
      let r = match op {
        Add => {
          guard args.length() == 2 else { fail!("add expects 2 args") }
          match (args[0], args[1]) {
            (Int32(x), Int32(y)) => Int32(x + y)
            (Double(x), Double(y)) => Double(x + y)
            _ => fail!("type mismatch")
          }
        }
        Sub => {
          guard args.length() == 2 else { fail!("sub expects 2 args") }
          match (args[0], args[1]) {
            (Int32(x), Int32(y)) => Int32(x - y)
            (Double(x), Double(y)) => Double(x - y)
            _ => fail!("type mismatch")
          }
        }
        Mul => {
          guard args.length() == 2 else { fail!("mul expects 2 args") }
          match (args[0], args[1]) {
            (Int32(x), Int32(y)) => Int32(x * y)
            (Double(x), Double(y)) => Double(x * y)
            _ => fail!("type mismatch")
          }
        }
        Div => {
          guard args.length() == 2 else { fail!("div expects 2 args") }
          match (args[0], args[1]) {
            (Int32(x), Int32(y)) => Int32(x / y)
            (Double(x), Double(y)) => Double(x / y)
            _ => fail!("type mismatch")
          }
        }
        Neg => {
          guard args.length() == 1 else { fail!("neg expects 1 arg") }
          match args[0] {
            Int32(x) => Int32(-x)
            Double(x) => Double(-x)
            _ => fail!("type mismatch")
          }
        }
        Lnot => {
          guard args.length() == 1 else { fail!("not expects 1 arg") }
          match args[0] {
            Bool(b) => Bool(not(b))
            _ => fail!("type mismatch")
          }
        }
        Eq => {
          guard args.length() == 2 else { fail!("eq expects 2 args") }
          match (args[0], args[1]) {
            (Int32(x), Int32(y)) => Bool(x == y)
            (Double(x), Double(y)) => Bool(x == y)
            (Bool(x), Bool(y)) => Bool(x == y)
            _ => fail!("type mismatch")
          }
        }
        Le => {
          guard args.length() == 2 else { fail!("le expects 2 args") }
          match (args[0], args[1]) {
            (Int32(x), Int32(y)) => Bool(x <= y)
            (Double(x), Double(y)) => Bool(x <= y)
            _ => fail!("type mismatch")
          }
        }
      }
      (ctx, r)
    }
    Call(~f, ~args, ..) => {
      let args = CoreInterpreter::eval_values!(ctx, args)
      let func = match ctx.funcs.get(f) {
        Some(func) => func
        None => return fail!("function not found \{f}")
      }
      (ctx, self.eval_fn!(ctx.set_function_args(args), func))
    }
    Apply(~f, ~args, ..) => {
      // Closure is a mem object, the first field is the function
      let clos = CoreInterpreter::eval_value!(ctx, f)
      let args = CoreInterpreter::eval_values!(ctx, args)
      match clos {
        Mem(~fields) =>
          match fields[0] {
            FnPtr(f) => {
              let func = match ctx.funcs.get(f) {
                Some(func) => func
                None => return fail!("function not found")
              }
              (
                ctx,
                self.eval_fn!(
                  ctx.set_function_args(args).set_closure(clos),
                  func,
                ),
              )
            }
            _ as f0 => fail!("not a function: \{f0}")
          }
        _ => fail!("not a closure")
      }
    }
    ExtCall(~ext_f, ~args, ..) => {
      let args = CoreInterpreter::eval_values!(ctx, args)
      match ext_f.substring(start="minimbt_".length(), end=ext_f.length()) {
        "print_int" => {
          guard args.length() == 1 else { fail!("print_int expects 1 arg") }
          match args[0] {
            Int32(x) => println("\{x}")
            _ => fail!("print_int expects Int")
          }
          (ctx, Unit)
        }
        "print_char" => {
          guard args.length() == 1 else { fail!("print_char expects 1 arg") }
          match args[0] {
            Int32(x) => println("\{Char::from_int(x)}")
            _ => fail!("print_char expects Int")
          }
          (ctx, Unit)
        }
        "print_endline" => {
          guard args.length() == 0 else {
            fail!("print_endline expects 0 args")
          }
          println("")
          (ctx, Unit)
        }
        "truncate" => {
          guard args.length() == 1 else { fail!("truncate expects 1 arg") }
          match args[0] {
            Double(d) => (ctx, Int32(d.to_int()))
            _ => fail!("truncate expects Double")
          }
        }
        "sin" => {
          guard args.length() == 1 else { fail!("sin expects 1 arg") }
          match args[0] {
            Double(d) => (ctx, Double(@math.sin(d)))
            _ => fail!("sin expects Double")
          }
        }
        "cos" => {
          guard args.length() == 1 else { fail!("cos expects 1 arg") }
          match args[0] {
            Double(d) => (ctx, Double(@math.cos(d)))
            _ => fail!("cos expects Double")
          }
        }
        "sqrt" => {
          guard args.length() == 1 else { fail!("sqrt expects 1 arg") }
          match args[0] {
            Double(d) => (ctx, Double(d.sqrt()))
            _ => fail!("sqrt expects Double")
          }
        }
        "abs_float" => {
          guard args.length() == 1 else { fail!("abs_float expects 1 arg") }
          match args[0] {
            Double(d) => (ctx, Double(d.abs()))
            _ => fail!("abs_float expects Double")
          }
        }
        "int_of_float" => {
          guard args.length() == 1 else { fail!("int_of_float expects 1 arg") }
          match args[0] {
            Double(d) => (ctx, Int32(d.to_int()))
            _ => fail!("int_of_float expects Double")
          }
        }
        "float_of_int" => {
          guard args.length() == 1 else { fail!("float_of_int expects 1 arg") }
          match args[0] {
            Int32(x) => (ctx, Double(x.to_double()))
            _ => fail!("float_of_int expects Int")
          }
        }
        _ => fail!("external function not found")
      }
    }
    LoadSymbol(sref=sym, ..) => {
      let obj = match ctx.global.find(sym) {
        Some(obj) => obj
        None => return fail!("symbol not found")
      }
      (ctx, obj)
    }
    Alloc(~mref, ~fills, ..) => {
      let mut ctx = ctx
      let mem = match func.mems.get(mref) {
        Some(mem) => mem
        None => return fail!("mem not found")
      }
      match mem {
        Heap(~len, ..) => {
          let len = CoreInterpreter::eval_value!(ctx, len)
          let obj = match len {
            Int32(len) => {
              let fields = Array::makei(len, fn(_i) { Uninit })
              Object::Mem(~fields)
            }
            _ => fail!("not an integer")
          }
          CoreInterpreter::eval_fills!(ctx, fills, obj)
          ctx = ctx.update_heap(func.id, mref, obj)
        }
        Stack(~len, ..) => {
          let len = CoreInterpreter::eval_value!(ctx, len)
          let obj = match len {
            Int32(len) => {
              let fields = Array::makei(len, fn(_i) { Uninit })
              Object::Mem(~fields)
            }
            _ => fail!("not an integer")
          }
          CoreInterpreter::eval_fills!(ctx, fills, obj)
          ctx = ctx.update_stack(mref, obj)
        }
      }
      (ctx, Object::Unit)
    }
    Store(~ma, ~val, ..) as st => {
      let val = CoreInterpreter::eval_value!(ctx, val)
      match ma {
        Field(val=dst, ~index, ..) => {
          let dst = CoreInterpreter::eval_value!(ctx, dst)
          match dst {
            Object::Mem(~fields) =>
              if index < fields.length() {
                fields[index] = val
              } else {
                fail!(
                  "field index out of bounds: \{index} out of \{fields.length()}",
                )
              }
            _ => fail!("eval \{st}: not a mem object: \{dst}, \{index}")
          }
        }
        Offset(val=dst, ~offset, ..) => {
          let dst = CoreInterpreter::eval_value!(ctx, dst)
          let offset = CoreInterpreter::eval_value!(ctx, offset)
          match (dst, offset) {
            (Object::Mem(~fields), Int32(n)) =>
              if n < fields.length() {
                fields[n] = val
              } else {
                fail!(
                  "field index out of bounds: \{n} out of \{fields.length()}",
                )
              }
            _ => fail!("eval \{st}: not a mem object: \{val}, \{offset}")
          }
        }
      }
      (ctx, Object::Unit)
    }
    Load(~ma, ..) => {
      let obj = match ma {
        Field(~val, ~index, ..) => {
          let val = CoreInterpreter::eval_value!(ctx, val)
          match val {
            Object::Mem(~fields) =>
              if index < fields.length() {
                fields[index]
              } else {
                fail!(
                  "field index out of bounds: \{index} out of \{fields.length()}",
                )
              }
            _ as m => fail!("not a mem object \{m}")
          }
        }
        Offset(val=dst, ~offset, ..) => {
          let m = CoreInterpreter::eval_value!(ctx, dst)
          let n = CoreInterpreter::eval_value!(ctx, offset)
          match (m, n) {
            (Object::Mem(~fields), Int32(n)) =>
              if n < fields.length() {
                fields[n]
              } else {
                fail!(
                  "field index out of bounds: \{n} out of \{fields.length()}",
                )
              }
            _ =>
              fail!(
                "not a mem object, or the offset is not an integer: \{m}, \{n}",
              )
          }
        }
      }
      (ctx, obj)
    }
  }
}

fn CoreInterpreter::eval_control(
  self : CoreInterpreter,
  ctx : Ctx,
  func : Fn,
  control : Control
) -> Object!Failure {
  match control {
    Jump(~target, ~args) => {
      let block = match func.blocks.get(target) {
        Some(block) => block
        None => return fail!("block not found: \{target}")
      }
      let args2 = CoreInterpreter::eval_values!(ctx, args)
      let ctx = ctx.set_block_args(target, args2)
      self.eval_block!(ctx, func, block)
    }
    Return(~args) => {
      guard args.length() == 1 else { fail!("multiple return values") }
      CoreInterpreter::eval_value!(ctx, args[0])
    }
    Branch(~cond, ~t, ~t_args, ~f, ~f_args) => {
      let cond = CoreInterpreter::eval_value!(ctx, cond)
      match cond {
        Bool(true) => {
          let args = CoreInterpreter::eval_values!(ctx, t_args)
          let block = match func.blocks.get(t) {
            Some(block) => block
            None => return fail!("block not found: \{t}")
          }
          let ctx = ctx.set_block_args(t, args)
          self.eval_block!(ctx, func, block)
        }
        Bool(false) => {
          let args = CoreInterpreter::eval_values!(ctx, f_args)
          let block = match func.blocks.get(f) {
            Some(block) => block
            None => return fail!("block not found: \{f}")
          }
          let ctx = ctx.set_block_args(f, args)
          self.eval_block!(ctx, func, block)
        }
        _ => fail!("not a boolean condition")
      }
    }
    Select(..) => fail!("Not implemented")
    Panic => fail!("panic")
  }
}

fn CoreInterpreter::eval_values(
  ctx : Ctx,
  values : Array[Value]
) -> Array[Object]!Failure {
  let objs = []
  for value in values {
    objs.push(CoreInterpreter::eval_value!(ctx, value))
  }
  objs
}

fn CoreInterpreter::eval_value(ctx : Ctx, value : Value) -> Object!Failure {
  match value {
    Unit => Unit
    Bool(~val) => Bool(val)
    Int32(~val) => Int32(val)
    Int64(~val) => Int64(val)
    Double(~val) => Double(val)
    Insn(~iref) => {
      let obj = match ctx.insns.find(iref) {
        Some(obj) => obj
        None => return fail!("insn not evaluated")
      }
      obj
    }
    Mem(~fref, ~mref) => {
      if trace {
        println(":::: stack \{ctx.stack}")
        println(":::: heap \{ctx.heap}")
      }
      let obj = match ctx.stack.find(mref) {
        Some(obj) => obj
        None =>
          match ctx.heap.get((fref, mref)) {
            Some(obj) => obj
            None => fail!("mem not in stack or heap")
          }
      }
      obj
    }
    Fn(~fref) => FnPtr(fref)
    FreeVar(fvref=FnFreeVarRef({ id }), ..) => {
      if trace {
        println(":::: closure \{ctx.closure}")
      }
      let fld = match ctx.closure {
        Mem(~fields) =>
          if id < fields.length() {
            fields[id]
          } else {
            fail!("free var not found: \{id} out of \{fields.length()}")
          }
        _ => fail!("unexpected closure: \{ctx.closure}")
      }
      fld
    }
    BlockParam(~bref, bpref=BlockParamRef({ id })) => {
      let blkargs = match ctx.blkargs.find(bref) {
        Some(args) => args
        None => fail!("block args not found")
      }
      if id < blkargs.length() {
        blkargs[id]
      } else {
        fail!("block param not found")
      }
    }
    FnParam(fpref=FnParamRef({ id }), ..) =>
      if id < ctx.fnargs.length() {
        ctx.fnargs[id]
      } else {
        fail!("fn param not found")
      }
    Self(_) => ctx.closure
  }
}
