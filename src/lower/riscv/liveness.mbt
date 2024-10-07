typealias AnyReg = @riscv.AnyReg

typealias Set[K] = @hashset.T[K]

pub struct BlockInfo {
  defs : Set[AnyReg] // defined in the block
  uses : Set[AnyReg] // used in the block
  upward_exposed : Set[AnyReg] // used but not defined in the block
  params_defs : Set[AnyReg] // block parameters defined
  params_uses : Set[AnyReg] // block parameters used
}

pub fn live_info(_insn : MInsn) -> (Set[AnyReg], Set[AnyReg]) {
  (Set::new(), Set::new())
}

pub fn BlockInfo::compute(_mbb : MBasicBlock) -> BlockInfo {
  let defs = Set::new()
  let uses = Set::new()
  let upward_exposed = Set::new()
  let params_defs = Set::new()
  let params_uses = Set::new()
  BlockInfo::{ defs, uses, upward_exposed, params_defs, params_uses }
}

pub struct Liveness {}
