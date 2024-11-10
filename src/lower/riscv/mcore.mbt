typealias Label = @riscv.Label

typealias MInsn = @riscv.RvAsm

enum GlobalVar {
  Zero(Int)
  Double(Double)
  Int(Int)
}

pub struct MCore {
  funcs : MutMap[Label, MFn]
  globals : MutMap[Label, GlobalVar]
  liveness : MutMap[Label, Liveness]
  primitives : MutSet[String]
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
  seq : Array[Label]
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
  for bref in self.seq {
    let block = self.blocks[bref].unwrap()
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
  for bref in self.seq {
    let block = self.blocks[bref].unwrap()
    block.print_with_info(logger, info.iter_block_info[block.id._].unwrap())
  }
}

pub fn MCore::output(self : MCore, logger : Logger) -> Unit {
  logger.write_string(
    "  .option arch, +xtheadba, +xtheadfmemidx, +xtheadmemidx\n",
  )
  logger.write_string(large_stack)
  logger.write_string("__begin_text:\n")
  for primitive in self.primitives {
    logger.write_string("\{primitive_map[primitive].unwrap()}\n")
  }
  //logger.write_string(my_rt)
  for fpair in self.funcs {
    let (_, func) = fpair
    logger.write_string("  .p2align 2\n")
    logger.write_string("  .text\n")
    logger.write_string("  .globl \{func.name._}\n")
    logger.write_string("  .type \{func.name._}, @function\n")
    func.output(logger)
  }
  logger.write_string("__end_text:\n")
  for global in self.globals {
    match global {
      (global, Zero(sz)) => {
        logger.write_string("  .p2align 2\n")
        logger.write_string("  .bss\n")
        logger.write_string("  .globl \{global._}\n")
        logger.write_string("  .type \{global._}, @object\n")
        logger.write_string("\{global._}:\n")
        logger.write_string("  .zero \{sz}\n\n")
      }
      (global, Double(val)) => {
        logger.write_string("  .p2align 3\n")
        logger.write_string("  .rodata\n")
        logger.write_string("  .globl \{global._}\n")
        logger.write_string("  .type \{global._}, @object\n")
        logger.write_string("\{global._}:\n")
        logger.write_string("  .double \{val}\n\n")
      }
      (global, Int(val)) => {
        logger.write_string("  .p2align 2\n")
        logger.write_string("  .rodata\n")
        logger.write_string("  .globl \{global._}\n")
        logger.write_string("  .type \{global._}, @object\n")
        logger.write_string("\{global._}:\n")
        logger.write_string("  .word \{val}\n\n")
      }
    }
  }
}

pub fn MCoreWithRichInfo::output(
  self : MCoreWithRichInfo,
  logger : Logger
) -> Unit {
  let self = self._
  for fpair in self.funcs {
    let (_, func) = fpair
    func.print_with_info(logger, self.liveness[func.name].unwrap())
  }
  for global in self.globals {
    match global {
      (global, Zero(sz)) => {
        logger.write_string("  .p2align 2\n")
        logger.write_string("  .bss\n")
        logger.write_string("\{global._}:\n")
        logger.write_string("  .zero \{sz}\n")
      }
      (global, Double(val)) => {
        logger.write_string("  .p2align 3\n")
        logger.write_string("  .rodata\n")
        logger.write_string("\{global._}:\n")
        logger.write_string("  .double \{val}\n")
      }
      (global, Int(val)) => {
        logger.write_string("  .p2align 2\n")
        logger.write_string("  .rodata\n")
        logger.write_string("\{global._}:\n")
        logger.write_string("  .word \{val}\n")
      }
    }
  }
}

let raw_memseti32 =
  #|  .p2align 2
  #|  .text
  #|  .globl memseti32
  #|  .type memseti32, @function
  #|memseti32:
  #|  blez a1, .memseti32_b1
  #|  slli a1, a1, 2
  #|  add a1, a1, a0
  #|.memseti32_b0:
  #|  sw a2, 0(a0)
  #|  addi a0, a0, 4
  #|  bne a0, a1, .memseti32_b0
  #|.memseti32_b1:
  #|  ret
  #|

let raw_memseti64 =
  #|  .p2align 2
  #|  .text
  #|  .globl memseti64
  #|  .type memseti64, @function
  #|memseti64:
  #|  blez a1, .memseti64_b1
  #|  slli a1, a1, 3
  #|  add a1, a1, a0
  #|.memseti64_b0:
  #|  sd a2, 0(a0)
  #|  addi a0, a0, 8
  #|  bne a0, a1, .memseti64_b0
  #|.memseti64_b1:
  #|  ret
  #|

let raw_memsetf64 =
  #|  .p2align 2
  #|  .text
  #|  .globl memsetf64
  #|  .type memsetf64, @function
  #|memsetf64:
  #|  fmv.x.d a2, fa0
  #|  tail memseti64
  #|

let raw_memset0e4 =
  #|  .p2align 2
  #|  .text
  #|  .globl memset0e4
  #|  .type memset0e4, @function
  #|memset0e4:
  #|  blez a1, .memset0e4_b1
  #|  slli a1, a1, 2
  #|  add a1, a1, a0
  #|.memset0e4_b0:
  #|  sw zero, 0(a0)
  #|  addi a0, a0, 4
  #|  bne a0, a1, .memset0e4_b0
  #|.memset0e4_b1:
  #|  ret
  #|

let raw_memset0e8 =
  #|  .p2align 2
  #|  .text
  #|  .globl memset0e8
  #|  .type memset0e8, @function
  #|memset0e8:
  #|  blez a1, .memset0e8_b1
  #|  slli a1, a1, 3
  #|  add a1, a1, a0
  #|.memset0e8_b0:
  #|  sd zero, 0(a0)
  #|  addi a0, a0, 8
  #|  bne a0, a1, .memset0e8_b0
  #|.memset0e8_b1:
  #|  ret
  #|

let primitive_map : Map[String, String] = {
  "memseti32": raw_memseti32,
  "memseti64": raw_memseti64,
  "memsetf64": raw_memsetf64,
  "memset0e4": raw_memset0e4,
  "memset0e8": raw_memset0e8,
}

let large_stack =
  #|  .p2align 4
  #|  .bss
  #|  .type large_heap, @object
  #|large_heap:
  #|  .zero 1610612736
  #|large_heap_end:
  #|
  #|  .p2align 4
  #|  .bss
  #|  .type large_stack, @object
  #|large_stack:
  #|  .zero 268435456
  #|large_stack_end:
  #|
  #|
