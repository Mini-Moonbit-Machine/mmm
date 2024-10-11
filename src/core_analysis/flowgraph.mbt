pub typealias MutMap[K, V] = @hashmap.T[K, V]

pub typealias MutSet[V] = @hashset.T[V]

struct CFGNode {
  predcessors : MutSet[@core.BlockRef]
  successors : MutSet[@core.BlockRef]
} derive(Show)

pub struct ControlFlowGraph {
  data : MutMap[@core.BlockRef, CFGNode]
} derive(Show)

pub fn ControlFlowGraph::new() -> ControlFlowGraph {
  ControlFlowGraph::{ data: MutMap::new() }
}

pub fn ControlFlowGraph::clear(self : ControlFlowGraph) -> Unit {
  self.data.clear()
}

pub fn ControlFlowGraph::with_function(func : @core.Fn) -> ControlFlowGraph {
  let cfg = ControlFlowGraph::new()
  cfg.compute(func)
  cfg
}

fn ControlFlowGraph::add_edge(
  self : ControlFlowGraph,
  from : @core.BlockRef,
  to : @core.BlockRef
) -> Unit {
  self.data.get(from).unwrap().predcessors.insert(to)
  self.data.get(to).unwrap().successors.insert(from)
}

pub fn ControlFlowGraph::compute(
  self : ControlFlowGraph,
  func : @core.Fn
) -> Unit {
  self.clear()
  for bpair in func.blocks {
    let (_, block) = bpair
    match block {
      @core.Block::BasicBlock(~bb) => {
        let succs = bb.control.successors()
        for succ in succs {
          self.add_edge(bb.id, succ)
        }
      }
      _ => () // ignore loop because its inner blocks are already in the map
    }
  }
}

pub typealias PredIter = Iter[@core.BlockRef]

pub typealias SuccIter = Iter[@core.BlockRef]

pub fn ControlFlowGraph::predcessors(
  self : ControlFlowGraph,
  block : @core.BlockRef
) -> MutSet[@core.BlockRef] {
  self.data.get(block).unwrap().predcessors
}

pub fn ControlFlowGraph::successors(
  self : ControlFlowGraph,
  block : @core.BlockRef
) -> MutSet[@core.BlockRef] {
  self.data.get(block).unwrap().successors
}
