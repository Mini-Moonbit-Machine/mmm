typealias BlockRef = @core.BlockRef

typealias BasicBlock = @core.BasicBlock

pub enum LoweredBlock {
  BasicBlock(~bb : BasicBlock)
  CriticalEdge(~pred : BasicBlock, ~succ : BasicBlock, ~succ_idx : Int)
}

pub fn LoweredBlock::to_string(self : LoweredBlock) -> String {
  match self {
    LoweredBlock::BasicBlock(~bb) => bb.id.to_string()
    LoweredBlock::CriticalEdge(~pred, ~succ, ~succ_idx) =>
      "\{pred.id} -> \{succ.id} (\{succ_idx})"
  }
}

impl Show for LoweredBlock with output(self, logger) {
  logger.write_string(LoweredBlock::to_string(self))
}

pub fn LoweredBlock::basic_block(self : LoweredBlock) -> BasicBlock? {
  match self {
    LoweredBlock::BasicBlock(~bb) => Some(bb)
    _ => None
  }
}

pub fn LoweredBlock::critical_edge(
  self : LoweredBlock
) -> (BasicBlock, BasicBlock, Int)? {
  match self {
    LoweredBlock::CriticalEdge(~pred, ~succ, ~succ_idx) =>
      Some((pred, succ, succ_idx))
    _ => None
  }
}

typealias MutMap[K, V] = @hashmap.T[K, V]

pub struct LoweringOrder {
  order : Array[LoweredBlock]
  succs : Array[Array[LoweredBlock]]
  color : MutMap[BlockRef, Int]
  seq : Array[BlockRef]
} derive(Show)

pub fn LoweringOrder::new(func : @core.Fn) -> LoweringOrder {
  let ordered_refs = @core_analysis.Ordering::dfs(func).rev_postorder()
  let ordered_blocks = ordered_refs.to_basic_blocks()
  let order = []
  let block_in_count = MutMap::from_iter(
    ordered_refs._.1.iter().map(fn(bref) { (bref, 0) }),
  )
  let block_out_count = MutMap::from_iter(
    ordered_refs._.1.iter().map(fn(bref) { (bref, 0) }),
  )
  let block_succs = MutMap::new()
  fn f_bb(bb : @core.BasicBlock) -> Unit {
    let succs = bb.control.successors()
    let succs = succs.map(
      fn(bref) { BasicBlock(bb=func.blocks[bref].unwrap().basic_block(func)) },
    )
    for succ in succs {
      let succ = succ.basic_block().unwrap()
      block_in_count[succ.id] = block_in_count[succ.id].unwrap() + 1
    }
    block_out_count[bb.id] = block_out_count[bb.id].unwrap() + succs.length()
    block_succs.set(bb.id, succs)
  }

  ordered_blocks._.each(f_bb)
  for bb in ordered_blocks._ {
    order.push(BasicBlock(~bb))
    // This block has multiple successors.
    if block_out_count[bb.id].unwrap() > 1 {
      let succs = block_succs[bb.id].unwrap()
      for succ_idx, succ in succs {
        let succ = succ.basic_block().unwrap()
        // The successor has multiple predecessors.
        if block_in_count[succ.id].unwrap() > 1 {
          let ce = CriticalEdge(pred=bb, ~succ, ~succ_idx)
          succs[succ_idx] = ce
          order.push(ce)
        }
      }
    }
  }
  let succs = []
  for bb in order {
    match bb {
      BasicBlock(~bb) => {
        let t = block_succs[bb.id].unwrap()
        succs.push(t)
      }
      CriticalEdge(~succ, ..) => succs.push([BasicBlock(bb=succ)])
    }
  }
  fn bb_metric(bb : @core.BasicBlock) -> Int {
    let x = match bb.control {
      @core.Control::Branch(..) => 110
      Select(..) => 120
      Jump(..) => 100
      Return(..) => 0
      Panic => 0
    }
    bb.seq.length() + x
  }

  let worklist = []
  let colored = MutMap::new()
  let seq = []
  fn mark_bb(bb : @core.BasicBlock, color : Int) {
    seq.push(bb.id)
    colored.set(bb.id, color)
    let succs = bb.control.successors()
    let succs = succs.map(
      fn(bref) { func.blocks[bref].unwrap().basic_block(func) },
    )
    let mut value = 0
    let mut selected_bb = None
    for succ in succs {
      let succ_value = bb_metric(succ)
      if succ_value > value && colored.contains(succ.id) == false {
        value = succ_value
        selected_bb = Some(succ)
      }
    }
    match selected_bb {
      Some(sbb) => {
        mark_bb(sbb, color)
        for succ in succs {
          if colored.contains(succ.id) == false && succ.id != sbb.id {
            worklist.push(succ)
          }
        }
      }
      None =>
        for succ in succs {
          if colored.contains(succ.id) == false {
            worklist.push(succ)
          }
        }
    }
  }

  worklist.push(func.blocks[func.entry].unwrap().basic_block(func))
  let mut used_color = 0
  while worklist.is_empty() == false {
    let bb = worklist.pop().unwrap()
    mark_bb(bb, used_color)
    used_color += 1
  }
  { order, succs, color: colored, seq }
}
