pub enum RvAsm {
  // reg-reg arith
  Add(~rd : Reg, ~rs1 : Reg, ~rs2 : Reg)
  Sub(~rd : Reg, ~rs1 : Reg, ~rs2 : Reg)
  Xor(~rd : Reg, ~rs1 : Reg, ~rs2 : Reg)
  Or(~rd : Reg, ~rs1 : Reg, ~rs2 : Reg)
  And(~rd : Reg, ~rs1 : Reg, ~rs2 : Reg)
  Sll(~rd : Reg, ~rs1 : Reg, ~rs2 : Reg)
  Srl(~rd : Reg, ~rs1 : Reg, ~rs2 : Reg)
  Sra(~rd : Reg, ~rs1 : Reg, ~rs2 : Reg)
  Slt(~rd : Reg, ~rs1 : Reg, ~rs2 : Reg)
  Sltu(~rd : Reg, ~rs1 : Reg, ~rs2 : Reg)
  // reg-imm arith
  Addi(~rd : Reg, ~rs1 : Reg, ~imm : Int)
  Xori(~rd : Reg, ~rs1 : Reg, ~imm : Int)
  Ori(~rd : Reg, ~rs1 : Reg, ~imm : Int)
  Andi(~rd : Reg, ~rs1 : Reg, ~imm : Int)
  Slli(~rd : Reg, ~rs1 : Reg, ~imm : Int)
  Srli(~rd : Reg, ~rs1 : Reg, ~imm : Int)
  Srai(~rd : Reg, ~rs1 : Reg, ~imm : Int)
  Slti(~rd : Reg, ~rs1 : Reg, ~imm : Int)
  Sltiu(~rd : Reg, ~rs1 : Reg, ~imm : Int)
  // mem
  // memory access
  Lb(~rd : Reg, ~mem : MemAccess[Reg, Int])
  Lh(~rd : Reg, ~mem : MemAccess[Reg, Int])
  Lw(~rd : Reg, ~mem : MemAccess[Reg, Int])
  Ld(~rd : Reg, ~mem : MemAccess[Reg, Int])
  Lbu(~rd : Reg, ~mem : MemAccess[Reg, Int])
  Lhu(~rd : Reg, ~mem : MemAccess[Reg, Int])
  Lwu(~rd : Reg, ~mem : MemAccess[Reg, Int])
  Sb(~rs : Reg, ~mem : MemAccess[Reg, Int])
  Sh(~rs : Reg, ~mem : MemAccess[Reg, Int])
  Sw(~rs : Reg, ~mem : MemAccess[Reg, Int])
  Sd(~rs : Reg, ~mem : MemAccess[Reg, Int])
  // control flow
  Beq(~rs1 : Reg, ~rs2 : Reg, ~target : Label)
  Bne(~rs1 : Reg, ~rs2 : Reg, ~target : Label)
  Blt(~rs1 : Reg, ~rs2 : Reg, ~target : Label)
  Bge(~rs1 : Reg, ~rs2 : Reg, ~target : Label)
  Ble(~rs1 : Reg, ~rs2 : Reg, ~target : Label)
  Bgt(~rs1 : Reg, ~rs2 : Reg, ~target : Label)
  Bltu(~rs1 : Reg, ~rs2 : Reg, ~target : Label)
  Bgeu(~rs1 : Reg, ~rs2 : Reg, ~target : Label)
  // system
  Ecall
  // rv32/64m
  Mul(~rd : Reg, ~rs1 : Reg, ~rs2 : Reg)
  Mulw(~rd : Reg, ~rs1 : Reg, ~rs2 : Reg)
  Mulh(~rd : Reg, ~rs1 : Reg, ~rs2 : Reg)
  Mulhsu(~rd : Reg, ~rs1 : Reg, ~rs2 : Reg)
  Mulhu(~rd : Reg, ~rs1 : Reg, ~rs2 : Reg)
  Div(~rd : Reg, ~rs1 : Reg, ~rs2 : Reg)
  Divw(~rd : Reg, ~rs1 : Reg, ~rs2 : Reg)
  Divu(~rd : Reg, ~rs1 : Reg, ~rs2 : Reg)
  Rem(~rd : Reg, ~rs1 : Reg, ~rs2 : Reg)
  Remw(~rd : Reg, ~rs1 : Reg, ~rs2 : Reg)
  Remu(~rd : Reg, ~rs1 : Reg, ~rs2 : Reg)
  // rvf
  FaddD(~rd : FReg, ~rs1 : FReg, ~rs2 : FReg)
  FsubD(~rd : FReg, ~rs1 : FReg, ~rs2 : FReg)
  FmulD(~rd : FReg, ~rs1 : FReg, ~rs2 : FReg)
  FdivD(~rd : FReg, ~rs1 : FReg, ~rs2 : FReg)
  Fld(~rd : FReg, ~mem : MemAccess[Reg, Int])
  Fsd(~rs : FReg, ~mem : MemAccess[Reg, Int])
  FbeqD(~rs1 : FReg, ~rs2 : FReg, ~target : Label)
  FbleD(~rs1 : FReg, ~rs2 : FReg, ~target : Label)
  FmvDX(~rd : FReg, ~rs : Reg)
  // pseudo instructions
  Nop
  La(~rd : Reg, ~l : Label)
  Li(~rd : Reg, ~i : String)
  Neg(~rd : Reg, ~rs : Reg)
  FnegD(~rd : FReg, ~rs : FReg)
  Mv(~rd : Reg, ~rs : Reg)
  FmvD(~rd : FReg, ~rs : FReg)
  J(~target : Label)
  Jalr(~target : Reg)
  Jr(~target : Reg)
  Call(~target : Label)
  Tail(~target : Label)
  Ret
  // Comments
  Label(String)
  Comment(String)
}

fn write3[TReg : Show](
  logger : Logger,
  op : String,
  rd : TReg,
  rs1 : TReg,
  rs2 : TReg
) -> Unit {
  logger.write_string(op)
  logger.write_string(" ")
  rd.output(logger)
  logger.write_string(", ")
  rs1.output(logger)
  logger.write_string(", ")
  rs2.output(logger)
}

fn write2imm[TReg : Show](
  logger : Logger,
  op : String,
  rd : TReg,
  rs1 : TReg,
  imm : Int
) -> Unit {
  logger.write_string(op)
  logger.write_string(" ")
  rd.output(logger)
  logger.write_string(", ")
  rs1.output(logger)
  logger.write_string(", ")
  logger.write_string(imm.to_string())
}

fn write2label[TReg : Show](
  logger : Logger,
  op : String,
  rs1 : TReg,
  rs2 : TReg,
  label : Label
) -> Unit {
  logger.write_string(op)
  logger.write_string(" ")
  rs1.output(logger)
  logger.write_string(", ")
  rs2.output(logger)
  logger.write_string(", ")
  logger.write_string(label._)
}

fn write2mem[TReg1 : Show, TReg2 : Show](
  logger : Logger,
  op : String,
  rd : TReg1,
  mem : MemAccess[TReg2, Int]
) -> Unit {
  logger.write_string(op)
  logger.write_string(" ")
  rd.output(logger)
  logger.write_string(", ")
  logger.write_string(mem.offset.to_string())
  logger.write_string("(")
  mem.base.output(logger)
  logger.write_string(")")
}

fn write2[TReg1 : Show, TReg2 : Show](
  logger : Logger,
  op : String,
  rd : TReg1,
  rs1 : TReg2
) -> Unit {
  logger.write_string(op)
  logger.write_string(" ")
  rd.output(logger)
  logger.write_string(", ")
  rs1.output(logger)
}

impl Show for RvAsm with output(self, logger) {
  match self {
    Label(_) => ()
    _ => logger.write_string("  ")
  }
  match self {
    Add(~rd, ~rs1, ~rs2) => write3(logger, "add", rd, rs1, rs2)
    Sub(~rd, ~rs1, ~rs2) => write3(logger, "sub", rd, rs1, rs2)
    Xor(~rd, ~rs1, ~rs2) => write3(logger, "xor", rd, rs1, rs2)
    Or(~rd, ~rs1, ~rs2) => write3(logger, "or", rd, rs1, rs2)
    And(~rd, ~rs1, ~rs2) => write3(logger, "and", rd, rs1, rs2)
    Sll(~rd, ~rs1, ~rs2) => write3(logger, "sll", rd, rs1, rs2)
    Srl(~rd, ~rs1, ~rs2) => write3(logger, "srl", rd, rs1, rs2)
    Sra(~rd, ~rs1, ~rs2) => write3(logger, "sra", rd, rs1, rs2)
    Slt(~rd, ~rs1, ~rs2) => write3(logger, "slt", rd, rs1, rs2)
    Sltu(~rd, ~rs1, ~rs2) => write3(logger, "sltu", rd, rs1, rs2)
    Addi(~rd, ~rs1, ~imm) => write2imm(logger, "addi", rd, rs1, imm)
    Xori(~rd, ~rs1, ~imm) => write2imm(logger, "xori", rd, rs1, imm)
    Ori(~rd, ~rs1, ~imm) => write2imm(logger, "ori", rd, rs1, imm)
    Andi(~rd, ~rs1, ~imm) => write2imm(logger, "andi", rd, rs1, imm)
    Slli(~rd, ~rs1, ~imm) => write2imm(logger, "slli", rd, rs1, imm)
    Srli(~rd, ~rs1, ~imm) => write2imm(logger, "srli", rd, rs1, imm)
    Srai(~rd, ~rs1, ~imm) => write2imm(logger, "srai", rd, rs1, imm)
    Slti(~rd, ~rs1, ~imm) => write2imm(logger, "slti", rd, rs1, imm)
    Sltiu(~rd, ~rs1, ~imm) => write2imm(logger, "sltiu", rd, rs1, imm)
    Lb(~rd, ~mem) => write2mem(logger, "lb", rd, mem)
    Lh(~rd, ~mem) => write2mem(logger, "lh", rd, mem)
    Lw(~rd, ~mem) => write2mem(logger, "lw", rd, mem)
    Ld(~rd, ~mem) => write2mem(logger, "ld", rd, mem)
    Lbu(~rd, ~mem) => write2mem(logger, "lbu", rd, mem)
    Lhu(~rd, ~mem) => write2mem(logger, "lhu", rd, mem)
    Lwu(~rd, ~mem) => write2mem(logger, "lwu", rd, mem)
    Sb(~rs, ~mem) => write2mem(logger, "sb", rs, mem)
    Sh(~rs, ~mem) => write2mem(logger, "sh", rs, mem)
    Sw(~rs, ~mem) => write2mem(logger, "sw", rs, mem)
    Sd(~rs, ~mem) => write2mem(logger, "sd", rs, mem)
    Beq(~rs1, ~rs2, ~target) => write2label(logger, "beq", rs1, rs2, target)
    Bne(~rs1, ~rs2, ~target) => write2label(logger, "bne", rs1, rs2, target)
    Blt(~rs1, ~rs2, ~target) => write2label(logger, "blt", rs1, rs2, target)
    Bge(~rs1, ~rs2, ~target) => write2label(logger, "bge", rs1, rs2, target)
    Ble(~rs1, ~rs2, ~target) => write2label(logger, "ble", rs1, rs2, target)
    Bgt(~rs1, ~rs2, ~target) => write2label(logger, "bgt", rs1, rs2, target)
    Bltu(~rs1, ~rs2, ~target) => write2label(logger, "bltu", rs1, rs2, target)
    Bgeu(~rs1, ~rs2, ~target) => write2label(logger, "bgeu", rs1, rs2, target)
    Ecall => logger.write_string("ecall")
    Mul(~rd, ~rs1, ~rs2) => write3(logger, "mul", rd, rs1, rs2)
    Mulw(~rd, ~rs1, ~rs2) => write3(logger, "mulw", rd, rs1, rs2)
    Mulh(~rd, ~rs1, ~rs2) => write3(logger, "mulh", rd, rs1, rs2)
    Mulhsu(~rd, ~rs1, ~rs2) => write3(logger, "mulhsu", rd, rs1, rs2)
    Mulhu(~rd, ~rs1, ~rs2) => write3(logger, "mulhu", rd, rs1, rs2)
    Div(~rd, ~rs1, ~rs2) => write3(logger, "div", rd, rs1, rs2)
    Divw(~rd, ~rs1, ~rs2) => write3(logger, "divw", rd, rs1, rs2)
    Divu(~rd, ~rs1, ~rs2) => write3(logger, "divu", rd, rs1, rs2)
    Rem(~rd, ~rs1, ~rs2) => write3(logger, "rem", rd, rs1, rs2)
    Remw(~rd, ~rs1, ~rs2) => write3(logger, "remw", rd, rs1, rs2)
    Remu(~rd, ~rs1, ~rs2) => write3(logger, "remu", rd, rs1, rs2)
    FaddD(~rd, ~rs1, ~rs2) => write3(logger, "fadd.d", rd, rs1, rs2)
    FsubD(~rd, ~rs1, ~rs2) => write3(logger, "fsub.d", rd, rs1, rs2)
    FmulD(~rd, ~rs1, ~rs2) => write3(logger, "fmul.d", rd, rs1, rs2)
    FdivD(~rd, ~rs1, ~rs2) => write3(logger, "fdiv.d", rd, rs1, rs2)
    Fld(~rd, ~mem) => write2mem(logger, "fld", rd, mem)
    Fsd(~rs, ~mem) => write2mem(logger, "fsd", rs, mem)
    FbeqD(~rs1, ~rs2, ~target) =>
      write2label(logger, "fbeq.d", rs1, rs2, target)
    FbleD(~rs1, ~rs2, ~target) =>
      write2label(logger, "fble.d", rs1, rs2, target)
    FmvDX(~rd, ~rs) => write2(logger, "fmv.d.x", rd, rs)
    Nop => logger.write_string("nop")
    La(~rd, ~l) => {
      logger.write_string("la ")
      Show::output(rd, logger)
      logger.write_string(", ")
      logger.write_string(l._)
    }
    Li(~rd, ~i) => {
      logger.write_string("li ")
      Show::output(rd, logger)
      logger.write_string(", ")
      logger.write_string(i)
    }
    Neg(~rd, ~rs) => write2(logger, "neg", rd, rs)
    FnegD(~rd, ~rs) => write2(logger, "fneg.d", rd, rs)
    Mv(~rd, ~rs) => write2(logger, "mv", rd, rs)
    FmvD(~rd, ~rs) => write2(logger, "fmv.d", rd, rs)
    J(~target) => {
      logger.write_string("j ")
      logger.write_string(target._)
    }
    Jalr(~target) => {
      logger.write_string("jalr ")
      Show::output(target, logger)
    }
    Jr(~target) => {
      logger.write_string("jr ")
      Show::output(target, logger)
    }
    Call(~target) => {
      logger.write_string("call ")
      logger.write_string(target._)
    }
    Tail(~target) => {
      logger.write_string("tail ")
      logger.write_string(target._)
    }
    Ret => logger.write_string("ret")
    Label(label) => {
      logger.write_string(label)
      logger.write_string(":")
    }
    Comment(comment) => {
      logger.write_string("# ")
      logger.write_string(comment)
    }
  }
}
