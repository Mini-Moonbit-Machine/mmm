typealias Label = @riscv.Label

typealias MInsn = @riscv.RvAsm

pub struct MCore {
  funcs : MutMap[Label, MFn]
  globals : MutMap[Label, Int]
}

pub struct MFn {
  name : Label
  entry : Label
  blocks : MutMap[Label, MBasicBlock]
}

pub struct MBasicBlock {
  id : Label
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

pub fn MFn::output(self : MFn, logger : Logger) -> Unit {
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

pub fn MCore::output(self : MCore, logger : Logger) -> Unit {
  for fpair in self.funcs {
    let (_, func) = fpair
    logger.write_string("  .p2align 3\n")
    logger.write_string("  .globl \{func.name._}\n")
    logger.write_string("  .type \{func.name._}, @function\n")
    logger.write_object(func)
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
