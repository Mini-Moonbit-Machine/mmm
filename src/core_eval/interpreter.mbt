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
  Int(Int)
  Bool(Bool)
  Double(Double)
  FnPtr(FnRef)
  Mem(mut ~fields : Array[Object])
} derive(Show)

pub struct Ctx {
  heap : MutMap[(FnRef, MemRef), Object]
  stack : Map[MemRef, Object]
  global : Map[FnRef, Object]
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

fn Ctx::update_global(self : Ctx, f : FnRef, obj : Object) -> Ctx {
  {
    global: self.global.add(f, obj),
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
  let { funcs, globals, .. } = core
  let mut ctx = Ctx::new(funcs)
  for thunk in globals {
    let (name, value) = thunk
    match value {
      Value::GlobalThunk(f) => {
        let func = match funcs.get(f) {
          Some(func) => func
          None => return fail!("function not found: \{name}")
        }
        let obj = self.eval_fn!(ctx, func)
        ctx = ctx.update_global(f, obj)
      }
      _ => fail!("unexpected global value")
    }
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
    BasicBlock(bb) => self.eval_bb!(ctx, func, bb)
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
          Zero => fields.map_inplace(fn(_i) { Int(0) })
          Init(v) => {
            let v = CoreInterpreter::eval_value!(ctx, v)
            fields.map_inplace(fn(_i) { v })
          }
          Field(i, v) => {
            let v = CoreInterpreter::eval_value!(ctx, v)
            if i < fields.length() {
              fields[i] = v
            } else {
              fail!("field index out of bounds: \{i} out of \{fields.length()}")
            }
            fields[i] = v
          }
          Offset(i, v) => {
            let i = CoreInterpreter::eval_value!(ctx, i)
            let v = CoreInterpreter::eval_value!(ctx, v)
            match (i, v) {
              (Int(i), v) =>
                if i < fields.length() {
                  fields[i] = v
                } else {
                  fail!(
                    "field index out of bounds: \{i} out of \{fields.length()}",
                  )
                }
              _ => fail!("not an integer offset")
            }
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
    Op(_, op, xs) => {
      let args = CoreInterpreter::eval_values!(ctx, xs)
      let r = match op {
        Add => {
          guard args.length() == 2 else { fail!("add expects 2 args") }
          match (args[0], args[1]) {
            (Int(x), Int(y)) => Int(x + y)
            (Double(x), Double(y)) => Double(x + y)
            _ => fail!("type mismatch")
          }
        }
        Sub => {
          guard args.length() == 2 else { fail!("sub expects 2 args") }
          match (args[0], args[1]) {
            (Int(x), Int(y)) => Int(x - y)
            (Double(x), Double(y)) => Double(x - y)
            _ => fail!("type mismatch")
          }
        }
        Mul => {
          guard args.length() == 2 else { fail!("mul expects 2 args") }
          match (args[0], args[1]) {
            (Int(x), Int(y)) => Int(x * y)
            (Double(x), Double(y)) => Double(x * y)
            _ => fail!("type mismatch")
          }
        }
        Div => {
          guard args.length() == 2 else { fail!("div expects 2 args") }
          match (args[0], args[1]) {
            (Int(x), Int(y)) => Int(x / y)
            (Double(x), Double(y)) => Double(x / y)
            _ => fail!("type mismatch")
          }
        }
        Neg => {
          guard args.length() == 1 else { fail!("neg expects 1 arg") }
          match args[0] {
            Int(x) => Int(-x)
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
            (Int(x), Int(y)) => Bool(x == y)
            (Double(x), Double(y)) => Bool(x == y)
            (Bool(x), Bool(y)) => Bool(x == y)
            _ => fail!("type mismatch")
          }
        }
        Le => {
          guard args.length() == 2 else { fail!("le expects 2 args") }
          match (args[0], args[1]) {
            (Int(x), Int(y)) => Bool(x <= y)
            (Double(x), Double(y)) => Bool(x <= y)
            _ => fail!("type mismatch")
          }
        }
      }
      (ctx, r)
    }
    Call(_, f, xs) => {
      let f = CoreInterpreter::eval_value!(ctx, f)
      let args = CoreInterpreter::eval_values!(ctx, xs)
      match f {
        FnPtr(f) => {
          let func = match ctx.funcs.get(f) {
            Some(func) => func
            None => return fail!("function not found \{f}")
          }
          (ctx, self.eval_fn!(ctx.set_function_args(args), func))
        }
        _ => fail!("not a function")
      }
    }
    Apply(_, clos, xs) => {
      // Closure is a mem object, the first field is the function
      let clos = CoreInterpreter::eval_value!(ctx, clos)
      let args = CoreInterpreter::eval_values!(ctx, xs)
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
    ExtCall(_, f, xs) => {
      let args = CoreInterpreter::eval_values!(ctx, xs)
      match f.substring(start="minimbt_".length(), end=f.length()) {
        "print_int" => {
          guard args.length() == 1 else { fail!("print_int expects 1 arg") }
          match args[0] {
            Int(x) => println("\{x}")
            _ => fail!("print_int expects Int")
          }
          (ctx, Unit)
        }
        "print_char" => {
          guard args.length() == 1 else { fail!("print_char expects 1 arg") }
          match args[0] {
            Int(x) => println("\{x.reinterpret_as_uint()}")
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
            Double(d) => (ctx, Int(d.to_int()))
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
            Double(d) => (ctx, Int(d.to_int()))
            _ => fail!("int_of_float expects Double")
          }
        }
        "float_of_int" => {
          guard args.length() == 1 else { fail!("float_of_int expects 1 arg") }
          match args[0] {
            Int(x) => (ctx, Double(x.to_double()))
            _ => fail!("float_of_int expects Int")
          }
        }
        _ => fail!("external function not found")
      }
    }
    Alloc(_, mr, fill) => {
      let mut ctx = ctx
      let mem = match func.mems.get(mr) {
        Some(mem) => mem
        None => return fail!("mem not found")
      }
      match mem {
        Heap(_, len) => {
          let len = CoreInterpreter::eval_value!(ctx, len)
          let obj = match len {
            Int(len) => {
              let fields = Array::makei(len, fn(_i) { Uninit })
              Object::Mem(~fields)
            }
            _ => fail!("not an integer")
          }
          CoreInterpreter::eval_fills!(ctx, fill, obj)
          ctx = ctx.update_heap(func.id, mr, obj)
        }
        Stack(_, len) => {
          let len = CoreInterpreter::eval_value!(ctx, len)
          let obj = match len {
            Int(len) => {
              let fields = Array::makei(len, fn(_i) { Uninit })
              Object::Mem(~fields)
            }
            _ => fail!("not an integer")
          }
          CoreInterpreter::eval_fills!(ctx, fill, obj)
          ctx = ctx.update_stack(mr, obj)
        }
      }
      (ctx, Object::Unit)
    }
    Store(_, ma, val) as st => {
      let val = CoreInterpreter::eval_value!(ctx, val)
      match ma {
        Field(_, m, n) => {
          let m = CoreInterpreter::eval_value!(ctx, m)
          match m {
            Object::Mem(~fields) =>
              if n < fields.length() {
                fields[n] = val
              } else {
                fail!(
                  "field index out of bounds: \{n} out of \{fields.length()}",
                )
              }
            _ => fail!("eval \{st}: not a mem object: \{m}, \{n}")
          }
        }
        Offset(_, m, n) => {
          let m = CoreInterpreter::eval_value!(ctx, m)
          let n = CoreInterpreter::eval_value!(ctx, n)
          match (m, n) {
            (Object::Mem(~fields), Int(n)) =>
              if n < fields.length() {
                fields[n] = val
              } else {
                fail!(
                  "field index out of bounds: \{n} out of \{fields.length()}",
                )
              }
            _ => fail!("eval \{st}: not a mem object: \{m}, \{n}")
          }
        }
      }
      (ctx, Object::Unit)
    }
    Load(_, ma) => {
      let obj = match ma {
        Field(_, m, n) => {
          let m = CoreInterpreter::eval_value!(ctx, m)
          match m {
            Object::Mem(~fields) =>
              if n < fields.length() {
                fields[n]
              } else {
                fail!(
                  "field index out of bounds: \{n} out of \{fields.length()}",
                )
              }
            _ as m => fail!("not a mem object \{m}")
          }
        }
        Offset(_, m, n) => {
          let m = CoreInterpreter::eval_value!(ctx, m)
          let n = CoreInterpreter::eval_value!(ctx, n)
          match (m, n) {
            (Object::Mem(~fields), Int(n)) =>
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
    Jump(blk, args) => {
      let block = match func.blocks.get(blk) {
        Some(block) => block
        None => return fail!("block not found: \{blk}")
      }
      let args2 = CoreInterpreter::eval_values!(ctx, args)
      let ctx = ctx.set_block_args(blk, args2)
      self.eval_block!(ctx, func, block)
    }
    Return(xs) => {
      guard xs.length() == 1 else { fail!("multiple return values") }
      CoreInterpreter::eval_value!(ctx, xs[0])
    }
    Branch(cond, t, targs, f, fargs) => {
      let cond = CoreInterpreter::eval_value!(ctx, cond)
      match cond {
        Bool(true) => {
          let args = CoreInterpreter::eval_values!(ctx, targs)
          let block = match func.blocks.get(t) {
            Some(block) => block
            None => return fail!("block not found: \{t}")
          }
          let ctx = ctx.set_block_args(t, args)
          self.eval_block!(ctx, func, block)
        }
        Bool(false) => {
          let args = CoreInterpreter::eval_values!(ctx, fargs)
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
    Select(_, _, _) => fail!("Not implemented")
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
    Bool(b) => Bool(b)
    Int(x) => Int(x)
    Double(x) => Double(x)
    Insn(ir) => {
      let obj = match ctx.insns.find(ir) {
        Some(obj) => obj
        None => return fail!("insn not evaluated")
      }
      obj
    }
    Mem(func, m) => {
      if trace {
        println(":::: stack \{ctx.stack}")
        println(":::: heap \{ctx.heap}")
      }
      let obj = match ctx.stack.find(m) {
        Some(obj) => obj
        None =>
          match ctx.heap.get((func, m)) {
            Some(obj) => obj
            None => fail!("mem not in stack or heap")
          }
      }
      obj
    }
    Fn(f) => FnPtr(f)
    GlobalThunk(f) => {
      let gbl = match ctx.global.find(f) {
        Some(gbl) => gbl
        None => return fail!("thunk not evaluated")
      }
      gbl
    }
    FreeVar(_, FnFreeVarRef({ id })) => {
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
    BlockParam(blk_ref, BlockParamRef({ id })) => {
      let blkargs = match ctx.blkargs.find(blk_ref) {
        Some(args) => args
        None => fail!("block args not found")
      }
      if id < blkargs.length() {
        blkargs[id]
      } else {
        fail!("block param not found")
      }
    }
    FnParam(_, FnParamRef({ id })) =>
      if id < ctx.fnargs.length() {
        ctx.fnargs[id]
      } else {
        fail!("fn param not found")
      }
    Self(_) => ctx.closure
  }
}
