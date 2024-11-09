typealias BlockRef = @core.BlockRef

typealias BasicBlock = @core.BasicBlock

pub enum LoweredBlock {
  BasicBlock(~bref : BlockRef)
  CriticalEdge(~pred : BlockRef, ~succ : BlockRef, ~succ_idx : Int)
} derive(Hash, Eq)

pub struct ColorSeq {
  color : Int
  seq : Int
  critical : Array[ColorSeq]
} derive(Show)

pub fn ColorSeq::op_equal(self : ColorSeq, other : ColorSeq) -> Bool {
  self.color == other.color &&
  self.seq < other.seq &&
  self.critical.filter(fn(cs) { cs.seq == other.seq }).length() == 0
}

pub fn LoweredBlock::to_string(self : LoweredBlock) -> String {
  match self {
    LoweredBlock::BasicBlock(~bref) => bref.id.to_string()
    LoweredBlock::CriticalEdge(~pred, ~succ, ~succ_idx) =>
      "\{pred.id} -> \{succ.id} (\{succ_idx})"
  }
}

pub impl Show for LoweredBlock with output(self, logger) {
  logger.write_string(LoweredBlock::to_string(self))
}

pub fn LoweredBlock::basic_block(self : LoweredBlock) -> BlockRef? {
  match self {
    LoweredBlock::BasicBlock(~bref) => Some(bref)
    _ => None
  }
}

pub fn LoweredBlock::critical_edge(
  self : LoweredBlock
) -> (BlockRef, BlockRef, Int)? {
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
  color_seq : MutMap[BlockRef, Int]
} derive(Show)

pub fn LoweringOrder::new(
  func : @core.Fn,
  tracer : @util.SubTracer
) -> LoweringOrder {
  let ordered_refs = @core_analysis.Ordering::dfs(func, tracer).rev_postorder()
  let ordered_blocks = ordered_refs.to_basic_blocks(tracer)
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
    let succs = succs.map(fn(bref) { BasicBlock(~bref) })
    for succ in succs {
      let succ_bref = succ.basic_block().unwrap()
      let succ = func.blocks.get(succ_bref).unwrap().bb_or_header_bb(func)
      block_in_count[succ.id] = block_in_count[succ.id].unwrap() + 1
    }
    block_out_count[bb.id] = block_out_count[bb.id].unwrap() + succs.length()
    block_succs.set(bb.id, succs)
  }

  ordered_blocks._.each(f_bb)
  for bb in ordered_blocks._ {
    order.push(BasicBlock(bref=bb.id))
    // This block has multiple successors.
    if block_out_count[bb.id].unwrap() > 1 {
      let succs = block_succs[bb.id].unwrap()
      for succ_idx, succ in succs {
        let succ_bref = succ.basic_block().unwrap()
        let succ = func.blocks.get(succ_bref).unwrap().bb_or_header_bb(func)
        // The successor has multiple predecessors.
        if block_in_count[succ.id].unwrap() > 1 {
          let ce = CriticalEdge(pred=bb.id, succ=succ_bref, ~succ_idx)
          succs[succ_idx] = ce
          order.push(ce)
        }
      }
    }
  }
  let succs = []
  let succs_map = MutMap::new()
  for lb in order {
    match lb {
      BasicBlock(~bref) => {
        let t = block_succs[bref].unwrap()
        succs.push(t)
        succs_map.set(lb, t)
      }
      CriticalEdge(~succ, ..) => {
        succs.push([BasicBlock(bref=succ)])
        succs_map.set(lb, [BasicBlock(bref=succ)])
      }
    }
  }
  fn bb_metric(bb : @core.BasicBlock) -> Int {
    let x = match bb.control {
      @core.Control::Branch(..) => 30
      Select(..) => 20
      Jump(..) => 10
      Return(..) => 0
      Panic => 0
    }
    bb.seq.length() + x
  }

  let worklist = @queue.new()
  let colored = MutMap::new()
  let seq = []
  let color_seq = MutMap::new()
  fn mark_bb(bb : BasicBlock, color : Int, n : Int) {
    if colored.contains(bb.id) {
      return
    }
    seq.push(bb.id)
    colored.set(bb.id, color)
    color_seq.set(bb.id, n)
    let succs = succs_map[BasicBlock(bref=bb.id)].unwrap()
    let mut value = -1
    let mut selected_bb = None
    for succ in succs {
      match succ {
        BasicBlock(~bref) => {
          let succ = func.blocks.get(bref).unwrap().bb_or_header_bb(func)
          let succ_value = bb_metric(succ)
          if succ_value > value && colored.contains(succ.id) == false {
            value = succ_value
            selected_bb = Some(succ)
          }
        }
        CriticalEdge(~succ, ..) =>
          if colored.contains(succ) == false {
            let bb = func.blocks.get(succ).unwrap().bb_or_header_bb(func)
            worklist.push(bb)
          }
      }
    }
    match selected_bb {
      Some(sbb) => {
        mark_bb(sbb, color, n + 1)
        for succ in succs {
          match succ {
            BasicBlock(~bref) =>
              if colored.contains(bref) == false && bref != sbb.id {
                let bb = func.blocks.get(bref).unwrap().bb_or_header_bb(func)
                worklist.push(bb)
              }
            _ => ()
          }
        }
      }
      None =>
        for succ in succs {
          match succ {
            BasicBlock(~bref) =>
              if colored.contains(bref) == false {
                let bb = func.blocks.get(bref).unwrap().bb_or_header_bb(func)
                worklist.push(bb)
              }
            _ => ()
          }
        }
    }
  }

  worklist.push(func.blocks[func.entry].unwrap().bb_or_header_bb(func))
  let mut used_color = 0
  while worklist.is_empty() == false {
    let bb = worklist.pop().unwrap()
    mark_bb(bb, used_color, 0)
    used_color += 1
  }
  tracer.println("LoweringOrder: \{order}")
  tracer.println("LoweringOrder: \{succs}")
  tracer.println("LoweringOrder: \{colored}")
  tracer.println("LoweringOrder: \{seq}")
  tracer.println("LoweringOrder: \{color_seq}")
  { order, succs, color: colored, seq, color_seq }
}
