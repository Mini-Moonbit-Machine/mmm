typealias Fn = @core.Fn

typealias BasicBlock = @core.BasicBlock

typealias Control = @core.Control

typealias Value = @core.Value

typealias Insn = @core.Insn

typealias BlockRef = @core.BlockRef

struct IfsDesc {
  tag : Value
  check_const_targets : Array[(BlockRef, Int, BlockRef)]
  term : BlockRef
} derive(Show)

struct SelectDesc {
  tag : Value
  deduce : Int
  cases : Array[BlockRef]
  fallback : BlockRef
} derive(Show)

struct FnJumpTable {
  func : @core.Fn
  tracer : @util.SubTracer
}

fn FnJumpTable::new(func : Fn, tracer : @util.SubTracer) -> FnJumpTable {
  { func, tracer }
}

fn is_const_eq_block(
  self : FnJumpTable,
  block : BasicBlock,
  tag : Value?,
  first : Bool
) -> ((Value, Int), (BlockRef, BlockRef))? {
  match block.control {
    Control::Branch(cond=Value::Insn(~iref), f_args=[], ~f, ~t, t_args=[]) => {
      let cond = self.func.insns.get(iref).unwrap()
      if not(first) && (block.seq.length() != 1 || block.seq[0] != iref) {
        return None
      }
      match cond {
        Insn::Op(op=@core.Op::Eq, args=[a1, a2], ..) =>
          if a1.is_const() {
            if tag.is_empty() || tag == Some(a2) {
              guard let Value::Int32(~val) = a1 else { _ => return None }
              Some(((a2, val), (t, f)))
            } else {
              None
            }
          } else if a2.is_const() {
            if tag.is_empty() || tag == Some(a1) {
              guard let Value::Int32(~val) = a2 else { _ => return None }
              Some(((a1, val), (t, f)))
            } else {
              None
            }
          } else {
            None
          }
        _ => None
      }
    }
    _ => None
  }
}

fn FnJumpTable::detect_consecutive_eq(
  self : FnJumpTable,
  start_block : BasicBlock
) -> IfsDesc? {
  let mut block = Some(start_block)
  let mut tag = None
  let mut term = None
  let table = []
  let checks = []
  while block.is_empty() == false {
    let bb = block.unwrap()
    match self.is_const_eq_block(bb, tag, bb.id == start_block.id) {
      Some(((tag2, const), (t, f))) => {
        table.push((bb.id, const, t))
        tag = Some(tag2)
        term = Some(f)
        checks.push(bb.id)
        block = Some(
          self.func.blocks.get(f).unwrap().bb_or_header_bb(self.func),
        )
      }
      None => block = None
    }
  }
  if table.length() <= 6 {
    return None
  }
  let constants = []
  let targets = []
  for item in table {
    let (_, const, t) = item
    constants.push(const)
    targets.push(t)
  }
  Some({ tag: tag.unwrap(), term: term.unwrap(), check_const_targets: table })
}

let max_jump_table_size = 100

fn make_select(self : FnJumpTable, ifs_desc : IfsDesc) -> SelectDesc? {
  let _ = self
  let check_const_targets = ifs_desc.check_const_targets.copy()
  check_const_targets.sort_by(
    fn(x, y) {
      let (_, a, _) = x
      let (_, b, _) = y
      a.compare(b)
    },
  )
  let mut min = check_const_targets[0].1
  let mut max = min
  let target_map = {}
  for item in check_const_targets {
    let (_, val, target) = item
    if val < min {
      min = val
    }
    if val > max {
      max = val
    }
    target_map.set(val, target)
  }
  if max - min + 1 > max_jump_table_size {
    return None
  }
  let deduce = min
  let cases = []
  self.tracer.println("\{min} \{max}")
  for i in min..=max {
    match target_map.get(i) {
      Some(target) => cases.push(target)
      None => cases.push(ifs_desc.term)
    }
  }
  self.tracer.println("\{cases}")
  Some({ tag: ifs_desc.tag, deduce, cases, fallback: ifs_desc.term })
}

fn FnJumpTable::go(self : FnJumpTable) -> Unit {
  let func = self.func
  let ordered = @core_analysis.Ordering::dfs(func, self.tracer)
    .rev_postorder()
    .to_basic_blocks(self.tracer)
  let desc = for bb in ordered._ {
    let x = self.detect_consecutive_eq(bb)
    match x {
      Some(desc) => break desc
      None => ()
    }
  } else {
    return
  }
  self.tracer.println("Jump table detected")
  self.tracer.println(desc)
  let first_check = desc.check_const_targets[0].0
  self.tracer.println("First check block: \{first_check}")
  let fn_counter = FnCounter::new_from_existing(func)
  match self.make_select(desc) {
    Some(select_desc) => {
      self.tracer.println("Jump table converted to select")
      self.tracer.println(select_desc)
      let fc_bb = func.blocks.get(first_check).unwrap().bb_or_header_bb(func)
      let deduce = Insn::Op(
        ty=@core.Type::Int32,
        op=@core.Op::Sub,
        args=[select_desc.tag, Value::Int32(val=select_desc.deduce)],
      )
      let deduce_iref = @core.Id::new(fn_counter.insn_counter)
      func.insns.set(deduce_iref, deduce)
      fc_bb.seq.push(deduce_iref)
      let control = Control::Select(
        cond=Value::Insn(iref=deduce_iref),
        targets=select_desc.cases,
        fallback=desc.term,
      )
      self.tracer.println("select control: \{control}")
      fc_bb.control = control
      // Remove the blocks that are no longer needed
      for x in 1..<desc.check_const_targets.length() {
        let (block, _, _) = desc.check_const_targets[x]
        self.func.blocks.remove(block)
      }
    }
    None => ()
  }
}

struct JumpTable {
  core : @core.Core
  tracer : @util.SubTracer
}

fn JumpTable::new(core : @core.Core, tracer : @util.SubTracer) -> JumpTable {
  { core, tracer }
}

fn JumpTable::go(self : JumpTable) -> Unit {
  for func in self.core.funcs {
    let (_, func) = func
    let jtbl = FnJumpTable::new(func, self.tracer)
    jtbl.go()
  }
}
