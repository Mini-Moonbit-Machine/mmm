typealias Label = @riscv.Label

typealias MInsn = @riscv.RvAsm

pub struct MCore {
  funcs : MutMap[Label, MFn]
  globals : MutMap[Label, Int]
  liveness : MutMap[Label, Liveness]
}

pub type MCoreWithRichInfo MCore

pub fn to_rich_info(self : MCore) -> MCoreWithRichInfo {
  self
}

pub struct MFn {
  name : Label
  leaf : Bool
  entry : Label
  blocks : MutMap[Label, MBasicBlock]
}

pub struct MBasicBlock {
  id : Label
  params : Array[AnyReg]
  insns : Array[MInsn]
  control : Array[MInsn]
}

pub fn MBasicBlock::output(self : MBasicBlock, logger : Logger) -> Unit {
  logger.write_string("\{self.id._}:\n")
  for insn in self.insns {
    logger.write_object(insn)
    logger.write_string("\n")
  }
  for insn in self.control {
    logger.write_object(insn)
    logger.write_string("\n")
  }
  logger.write_string("\n")
}

pub fn MBasicBlock::print_with_info(
  self : MBasicBlock,
  logger : Logger,
  info : IteratedBlockInfo
) -> Unit {
  let live_in = info.live_in.iter().to_string()
  let live_out = info.live_out.iter().to_string()
  logger.write_string("# block info: \{info.block_info}\n")
  logger.write_string("# live_in: \{live_in}, live_out: \{live_out}\n")
  let params = self.params.iter().to_string()
  logger.write_string("# block parameters: \{params}\n")
  logger.write_string("\{self.id._}:\n")
  let mut i = 0
  for insn in self.insns {
    logger.write_object(insn)
    logger.write_string(
      "  # live: \{info.live_set[i + 1].iter().to_string()}\n",
    )
    i += 1
  }
  logger.write_string("# control\n")
  for insn in self.control {
    logger.write_object(insn)
    logger.write_string(
      "  # live: \{info.live_set[i + 1].iter().to_string()}\n",
    )
    i += 1
  }
  logger.write_string("\n")
}

pub fn MFn::output(self : MFn, logger : Logger) -> Unit {
  logger.write_string("# leaf \{self.leaf}\n")
  logger.write_string("\{self.name._}:\n")
  let entry = self.entry
  let block = self.blocks[entry].unwrap()
  logger.write_object(block)
  for block in self.blocks {
    let (_, block) = block
    if block.id == entry {
      continue
    }
    logger.write_object(block)
  }
}

pub fn MFn::print_with_info(
  self : MFn,
  logger : Logger,
  info : Liveness
) -> Unit {
  logger.write_string("# leaf \{self.leaf}\n")
  logger.write_string("\{self.name._}:\n")
  let entry = self.entry
  let block = self.blocks[entry].unwrap()
  block.print_with_info(logger, info.iter_block_info[entry._].unwrap())
  for block in self.blocks {
    let (_, block) = block
    if block.id == entry {
      continue
    }
    block.print_with_info(logger, info.iter_block_info[block.id._].unwrap())
  }
}

pub fn MCore::output(self : MCore, logger : Logger) -> Unit {
  for fpair in self.funcs {
    let (_, func) = fpair
    logger.write_string("  .p2align 3\n")
    logger.write_string("  .globl \{func.name._}\n")
    logger.write_string("  .type \{func.name._}, @function\n")
    func.output(logger)
  }
  for global in self.globals {
    let (global, sz) = global
    logger.write_string("  .p2align 3\n")
    logger.write_string("  .globl \{global._}\n")
    logger.write_string("  .type \{global._}, @object\n")
    logger.write_string("\{global._}:\n")
    logger.write_string("  .zero \{sz}\n")
  }
}

pub fn MCoreWithRichInfo::output(
  self : MCoreWithRichInfo,
  logger : Logger
) -> Unit {
  let self = self._
  for fpair in self.funcs {
    let (_, func) = fpair
    logger.write_string("  .p2align 3\n")
    logger.write_string("  .globl \{func.name._}\n")
    logger.write_string("  .type \{func.name._}, @function\n")
    func.print_with_info(
      logger,
      self.liveness[func.name].or_else(
        fn() { abort("liveness not found: \{func.name}") },
      ),
    )
  }
  for global in self.globals {
    let (global, sz) = global
    logger.write_string("  .p2align 3\n")
    logger.write_string("  .globl \{global._}\n")
    logger.write_string("  .type \{global._}, @object\n")
    logger.write_string("\{global._}:\n")
    logger.write_string("  .zero \{sz}\n")
  }
}
