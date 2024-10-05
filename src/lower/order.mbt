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
} derive(Show)

pub fn LoweringOrder::new(func : @core.Fn) -> LoweringOrder {
  let ordered_refs = @core_analysis.Ordering::dfs(func).rev_postorder()
  let ordered_blocks = ordered_refs.to_basic_blocks(func)
  let order = []
  let block_in_count = MutMap::from_iter(
    ordered_refs._.iter().map(fn(bref) { (bref, 0) }),
  )
  let block_out_count = MutMap::from_iter(
    ordered_refs._.iter().map(fn(bref) { (bref, 0) }),
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
  { order, succs }
}
