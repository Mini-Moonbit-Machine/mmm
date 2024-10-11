typealias Label = @riscv.Label

typealias MInsn = @riscv.RvAsm

pub struct MCore {
  funcs : MutMap[Label, MFn]
  globals : MutMap[Label, Int]
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
  logger.write_string(large_stack)
  for primitive in self.primitives {
    logger.write_string("\{primitive_map[primitive].unwrap()}\n")
  }
  logger.write_string(my_rt)
  for fpair in self.funcs {
    let (_, func) = fpair
    logger.write_string("  .p2align 2\n")
    logger.write_string("  .text\n")
    logger.write_string("  .globl \{func.name._}\n")
    logger.write_string("  .type \{func.name._}, @function\n")
    func.output(logger)
  }
  for global in self.globals {
    let (global, sz) = global
    logger.write_string("  .p2align 2\n")
    logger.write_string("  .bss\n")
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
    logger.write_string("  .p2align 2\n")
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
    logger.write_string("  .p2align 2\n")
    logger.write_string("  .globl \{global._}\n")
    logger.write_string("  .type \{global._}, @object\n")
    logger.write_string("\{global._}:\n")
    logger.write_string("  .zero \{sz}\n")
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
  #|  slli a1, a1, 2
  #|  add a1, a1, a0
  #|.memseti64_b0:
  #|  sd a2, 0(a0)
  #|  addi a0, a0, 4
  #|  bne a0, a1, .memseti64_b0
  #|.memseti64_b1:
  #|  ret
  #|
  #|

let raw_memsetf64 =
  #|  .p2align 2
  #|  .text
  #|  .globl memsetf64
  #|  .type memsetf64, @function
  #|memsetf64:
  #|  blez a1, .memsetf64_b1
  #|  slli a1, a1, 3
  #|  add a1, a1, a0
  #|.memsetf64_b0:
  #|  fsd fa0, 0(a0)
  #|  addi a0, a0, 8
  #|  bne a0, a1, .memsetf64_b0
  #|.memsetf64_b1:
  #|  ret
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
  #|  fsd zero, 0(a0)
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
  #|  .type large_stack, @object
  #|large_stack:
  #|  .zero 1073741824
  #|large_stack_end:
  #|  .zero 16
  #|

let my_rt =
  #|  .p2align 2
  #|  .text
  #|  .globl my_read_int
  #|  .type my_read_int, @function
  #|my_read_int:
  #|  addi sp, sp, -528
  #|  li a4, 0
  #|  li t0, -1
  #|  lui a5, 1048575
  #|  li a3, 4
  #|  addi a6, sp, 12
  #|  li t1, 10
  #|  li t2, 512
  #|  li a7, 63
  #|.LBB0_1:
  #|  addi a1, sp, 524
  #|  li a2, 1
  #|  li a0, 0
  #|  ecall
  #|  sltu a1, a5, a0
  #|  negw a2, a0
  #|  negw a1, a1
  #|  and a1, a1, a2
  #|  slli a1, a1, 48
  #|  srli a1, a1, 48
  #|  beq a1, a3, .LBB0_1
  #|  bnez a1, .LBB0_58
  #|  beqz a0, .LBB0_6
  #|  lbu a1, 524(sp)
  #|  add a0, a6, a4
  #|  beq a1, t1, .LBB0_8
  #|  sb a1, 0(a0)
  #|  addi a4, a4, 1
  #|  addi t0, t0, 1
  #|  bne a4, t2, .LBB0_1
  #|  j .LBB0_58
  #|.LBB0_6:
  #|  beqz a4, .LBB0_58
  #|  addi a0, sp, 12
  #|  add a0, a0, a4
  #|  li a1, 10
  #|  sb a1, 0(a0)
  #|  j .LBB0_9
  #|.LBB0_8:
  #|  li a1, 10
  #|  sb a1, 0(a0)
  #|  beqz a4, .LBB0_14
  #|.LBB0_9:
  #|  lbu a0, 12(sp)
  #|  li a1, 95
  #|  beq a0, a1, .LBB0_15
  #|  li a1, 45
  #|  beq a0, a1, .LBB0_16
  #|  li a1, 43
  #|  bne a0, a1, .LBB0_18
  #|  lbu a0, 13(sp)
  #|  addi a1, a4, -1
  #|  seqz a1, a1
  #|  addi a0, a0, -95
  #|  seqz a0, a0
  #|  or a1, a1, a0
  #|  li a0, 15
  #|  slli t1, a0, 32
  #|  beqz a1, .LBB0_29
  #|  li a4, 0
  #|  j .LBB0_56
  #|.LBB0_14:
  #|  li a0, 15
  #|  slli a0, a0, 32
  #|  j .LBB0_57
  #|.LBB0_15:
  #|  li a5, 0
  #|  li t0, 15
  #|  slli t0, t0, 32
  #|  j .LBB0_52
  #|.LBB0_16:
  #|  lbu a0, 13(sp)
  #|  addi a1, a4, -1
  #|  seqz a1, a1
  #|  addi a0, a0, -95
  #|  seqz a0, a0
  #|  or a0, a0, a1
  #|  li t2, 15
  #|  slli t2, t2, 32
  #|  beqz a0, .LBB0_40
  #|  li a4, 0
  #|  j .LBB0_54
  #|.LBB0_18:
  #|  addi a1, sp, 12
  #|  add a0, a4, a1
  #|  lbu a0, -1(a0)
  #|  li t0, 15
  #|  li t1, 95
  #|  slli t0, t0, 32
  #|  li a5, 0
  #|  beq a0, t1, .LBB0_52
  #|  li a6, 9
  #|  li a7, 10
  #|  j .LBB0_23
  #|.LBB0_20:
  #|  li a5, 0
  #|.LBB0_21:
  #|  sext.w a0, a5
  #|  add a5, a0, a3
  #|  addw a0, a0, a3
  #|  xor a2, a0, a5
  #|  snez a2, a2
  #|  sb a2, 524(sp)
  #|  bne a0, a5, .LBB0_27
  #|.LBB0_22:
  #|  addi a4, a4, -1
  #|  addi a1, a1, 1
  #|  beqz a4, .LBB0_51
  #|.LBB0_23:
  #|  lbu a0, 0(a1)
  #|  beq a0, t1, .LBB0_22
  #|  addi a0, a0, -48
  #|  andi a3, a0, 255
  #|  bltu a6, a3, .LBB0_28
  #|  sext.w a0, a5
  #|  beqz a0, .LBB0_20
  #|  mul a5, a0, a7
  #|  mulw a2, a0, a7
  #|  xor a0, a2, a5
  #|  snez a0, a0
  #|  sb a0, 524(sp)
  #|  beq a2, a5, .LBB0_21
  #|.LBB0_27:
  #|  li t0, 7
  #|  slli t0, t0, 33
  #|.LBB0_28:
  #|  j .LBB0_52
  #|.LBB0_29:
  #|  addi a1, sp, 13
  #|  add a4, a4, a1
  #|  lbu a3, -2(a4)
  #|  li a4, 0
  #|  li a2, 95
  #|  beq a3, a2, .LBB0_56
  #|  li a0, 15
  #|  slli t1, a0, 32
  #|  li a6, 9
  #|  li a7, 10
  #|  j .LBB0_34
  #|.LBB0_31:
  #|  li a0, 0
  #|.LBB0_32:
  #|  sext.w a0, a0
  #|  add a4, a0, a3
  #|  addw a0, a0, a3
  #|  xor a3, a0, a4
  #|  snez a3, a3
  #|  sb a3, 524(sp)
  #|  bne a0, a4, .LBB0_38
  #|.LBB0_33:
  #|  addi t0, t0, -1
  #|  addi a1, a1, 1
  #|  beqz t0, .LBB0_55
  #|.LBB0_34:
  #|  lbu a3, 0(a1)
  #|  beq a3, a2, .LBB0_33
  #|  addi a3, a3, -48
  #|  andi a3, a3, 255
  #|  bltu a6, a3, .LBB0_39
  #|  sext.w a5, a4
  #|  beqz a5, .LBB0_31
  #|  mul a0, a5, a7
  #|  mulw a5, a5, a7
  #|  xor a4, a5, a0
  #|  snez a4, a4
  #|  sb a4, 524(sp)
  #|  beq a5, a0, .LBB0_32
  #|.LBB0_38:
  #|  li t1, 7
  #|  slli t1, t1, 33
  #|.LBB0_39:
  #|  j .LBB0_56
  #|.LBB0_40:
  #|  addi a1, sp, 13
  #|  add a4, a4, a1
  #|  lbu a0, -2(a4)
  #|  li a4, 0
  #|  li a2, 95
  #|  beq a0, a2, .LBB0_54
  #|  li t2, 15
  #|  slli t2, t2, 32
  #|  li a7, 9
  #|  li a6, 48
  #|  li t1, 10
  #|  j .LBB0_44
  #|.LBB0_42:
  #|  subw a0, a6, a5
  #|  slli a0, a0, 56
  #|  srai a4, a0, 56
  #|.LBB0_43:
  #|  addi t0, t0, -1
  #|  addi a1, a1, 1
  #|  beqz t0, .LBB0_53
  #|.LBB0_44:
  #|  lbu a5, 0(a1)
  #|  beq a5, a2, .LBB0_43
  #|  addi a0, a5, -48
  #|  andi a3, a0, 255
  #|  bltu a7, a3, .LBB0_50
  #|  sext.w a0, a4
  #|  beqz a0, .LBB0_42
  #|  mul a4, a0, t1
  #|  mulw a0, a0, t1
  #|  xor a5, a0, a4
  #|  snez a5, a5
  #|  sb a5, 524(sp)
  #|  bne a0, a4, .LBB0_49
  #|  sext.w a0, a4
  #|  sub a4, a0, a3
  #|  subw a0, a0, a3
  #|  xor a3, a0, a4
  #|  snez a3, a3
  #|  sb a3, 524(sp)
  #|  beq a0, a4, .LBB0_43
  #|.LBB0_49:
  #|  li t2, 7
  #|  slli t2, t2, 33
  #|.LBB0_50:
  #|  j .LBB0_54
  #|.LBB0_51:
  #|  li t0, 0
  #|.LBB0_52:
  #|  slli a5, a5, 32
  #|  srli a5, a5, 32
  #|  or a0, t0, a5
  #|  j .LBB0_57
  #|.LBB0_53:
  #|  li t2, 0
  #|.LBB0_54:
  #|  slli a4, a4, 32
  #|  srli a4, a4, 32
  #|  or a0, t2, a4
  #|  j .LBB0_57
  #|.LBB0_55:
  #|  li t1, 0
  #|.LBB0_56:
  #|  slli a4, a4, 32
  #|  srli a4, a4, 32
  #|  or a0, t1, a4
  #|.LBB0_57:
  #|  li a1, 15
  #|  slli a1, a1, 32
  #|  and a1, a1, a0
  #|  beqz a1, .LBB0_59
  #|.LBB0_58:
  #|  lui a0, 524288
  #|.LBB0_59:
  #|  sext.w a0, a0
  #|  addi sp, sp, 528
  #|  ret
  #|
  #|  .p2align 2
  #|  .text
  #|  .globl my_read_char
  #|  .type my_read_char, @function
  #|my_read_char:
  #|  addi sp, sp, -16
  #|  lui a3, 1048575
  #|  li a4, 4
  #|  li a7, 63
  #|.LBB1_1:
  #|  addi a1, sp, 15
  #|  li a2, 1
  #|  li a0, 0
  #|  ecall
  #|  sltu a1, a3, a0
  #|  negw a2, a0
  #|  negw a1, a1
  #|  and a1, a1, a2
  #|  slli a1, a1, 48
  #|  srli a1, a1, 48
  #|  beq a1, a4, .LBB1_1
  #|  bnez a1, .LBB1_4
  #|  li a1, 1
  #|  j .LBB1_5
  #|.LBB1_4:
  #|  li a1, 0
  #|.LBB1_5:
  #|  lbu a2, 15(sp)
  #|  snez a0, a0
  #|  and a0, a0, a1
  #|  addi a0, a0, -1
  #|  or a0, a0, a2
  #|  addi sp, sp, 16
  #|  ret
  #|
  #|  .p2align 2
  #|  .text
  #|  .globl my_print_char
  #|  .type my_print_char,@function  
  #|my_print_char:
  #|  addi sp, sp, -16
  #|  sb a0, 15(sp)
  #|  li a7, 64
  #|  li a0, 1
  #|  addi a1, sp, 15
  #|  li a2, 1
  #|  ecall
  #|  addi sp, sp, 16
  #|  ret
  #|  .p2align 2
  #|  .text
  #|  .globl my_print_endline
  #|  .type my_print_endline,@function  
  #|  .globl my_print_newline
  #|  .type my_print_newline,@function
  #|my_print_newline:
  #|my_print_endline:
  #|  li a0, 10
  #|  tail my_print_char
  #|
  #|  .p2align 2
  #|  .text
  #|  .globl my_print_int
  #|  .type my_print_int,@function  
  #|my_print_int:
  #|  addi sp, sp, -64
  #|  sd ra, 56(sp)
  #|  sd s0, 48(sp)
  #|  sd s1, 40(sp)
  #|  sraiw a1, a0, 31
  #|  xor a3, a0, a1
  #|  subw a3, a3, a1
  #|  li a1, 32
  #|  lui a2, 838861
  #|  addiw a2, a2, -819
  #|  slli a2, a2, 32
  #|  li a6, 10
  #|  addi a7, sp, 7
  #|  li s0, 9
  #|.LBB4_1:
  #|  mv s1, a3
  #|  slli a3, a3, 32
  #|  mulhu a3, a3, a2
  #|  srli a3, a3, 35
  #|  mul a4, a3, a6
  #|  subw a4, s1, a4
  #|  add a5, a7, a1
  #|  ori a4, a4, 48
  #|  sb a4, 0(a5)
  #|  addi a1, a1, -1
  #|  bltu s0, s1, .LBB4_1
  #|  bltz a0, .LBB4_4
  #|  addi a1, a1, 1
  #|  addi s0, a1, -33
  #|  bnez s0, .LBB4_5
  #|  j .LBB4_7
  #|.LBB4_4:
  #|  addi a0, sp, 7
  #|  add a0, a0, a1
  #|  li a2, 45
  #|  sb a2, 0(a0)
  #|  addi s0, a1, -33
  #|  beqz s0, .LBB4_7
  #|.LBB4_5:
  #|  addi s1, sp, 7
  #|  add s1, s1, a1
  #|.LBB4_6:
  #|  lbu a0, 0(s1)
  #|  call my_print_char
  #|  addi s0, s0, 1
  #|  addi s1, s1, 1
  #|  bnez s0, .LBB4_6
  #|.LBB4_7:
  #|  ld ra, 56(sp)
  #|  ld s0, 48(sp)
  #|  ld s1, 40(sp)
  #|  addi sp, sp, 64
  #|  ret
  #|
