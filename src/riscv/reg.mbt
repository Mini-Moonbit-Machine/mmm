pub struct MemAccess[TBase, TOff] {
  base : TBase
  offset : TOff
} derive(Show)

pub type Label String derive(Show, Eq, Hash)

pub fn abort[T](msg : String) -> T {
  println(msg)
  panic()
}

pub enum RegClass {
  X
  F
} derive(Eq, Compare, Hash)

pub struct AnyReg {
  mut reg : Int
  class : RegClass
} derive(Eq, Compare, Hash)

pub fn AnyReg::output(self : AnyReg, logger : Logger) -> Unit {
  logger.write_string(self.pretty())
}

pub fn AnyReg::to_string(self : AnyReg) -> String {
  self.pretty()
}

pub fn AnyReg::pretty(self : AnyReg) -> String {
  match self.class {
    X => "v\{self.reg}"
    F => "fv\{self.reg}"
  }
}

pub fn any_reg(reg : Int, class : RegClass) -> AnyReg {
  { reg, class }
}

pub fn any_xreg(reg : Int) -> AnyReg {
  { reg, class: X }
}

pub fn any_freg(reg : Int) -> AnyReg {
  { reg, class: F }
}

pub type Imm20 Int

pub type Imm12 Int

/// RISC-V register
pub enum Reg {
  Zero
  Ra
  Sp
  Gp
  Tp
  T0
  T1
  T2
  Fp
  S1
  A0
  A1
  A2
  A3
  A4
  A5
  A6
  A7
  S2
  S3
  S4
  S5
  S6
  S7
  S8
  S9
  S10
  S11
  T3
  T4
  T5
  T6
  V(AnyReg)
} derive(Eq, Compare, Hash)

pub fn phy_to_virt(self : Reg) -> AnyReg {
  match self {
    V(r) => r
    Zero => any_xreg(-1)
    Ra => any_xreg(-2)
    Sp => any_xreg(-3)
    Gp => any_xreg(-4)
    Tp => any_xreg(-5)
    T0 => any_xreg(-6)
    T1 => any_xreg(-7)
    T2 => any_xreg(-8)
    Fp => any_xreg(-9)
    S1 => any_xreg(-10)
    A0 => any_xreg(-11)
    A1 => any_xreg(-12)
    A2 => any_xreg(-13)
    A3 => any_xreg(-14)
    A4 => any_xreg(-15)
    A5 => any_xreg(-16)
    A6 => any_xreg(-17)
    A7 => any_xreg(-18)
    S2 => any_xreg(-19)
    S3 => any_xreg(-20)
    S4 => any_xreg(-21)
    S5 => any_xreg(-22)
    S6 => any_xreg(-23)
    S7 => any_xreg(-24)
    S8 => any_xreg(-25)
    S9 => any_xreg(-26)
    S10 => any_xreg(-27)
    S11 => any_xreg(-28)
    T3 => any_xreg(-29)
    T4 => any_xreg(-30)
    T5 => any_xreg(-31)
    T6 => any_xreg(-32)
  }
}

pub fn virt_to_phy(self : AnyReg) -> Reg {
  let r = match self.reg {
    -1 => Zero
    -2 => Ra
    -3 => Sp
    -4 => Gp
    -5 => Tp
    -6 => T0
    -7 => T1
    -8 => T2
    -9 => Fp
    -10 => S1
    -11 => A0
    -12 => A1
    -13 => A2
    -14 => A3
    -15 => A4
    -16 => A5
    -17 => A6
    -18 => A7
    -19 => S2
    -20 => S3
    -21 => S4
    -22 => S5
    -23 => S6
    -24 => S7
    -25 => S8
    -26 => S9
    -27 => S10
    -28 => S11
    -29 => T3
    -30 => T4
    -31 => T5
    -32 => T6
    _ =>
      if self.reg >= 0 && self.class == X {
        V(self)
      } else {
        abort("invalid register \{self} to integer register")
      }
  }
  r
}

pub let reg_temp_list : Array[Reg] = [T0, T1, T2, T3, T4, T5]

pub let reg_saved_list : Array[Reg] = [
  // S0, <- fp
  S1,
  S2,
  S3,
  S4,
  S5,
  S6,
  S7,
  S8,
  S9,
  S10,
  S11,
]

pub let reg_arg_list : Array[Reg] = [A0, A1, A2, A3, A4, A5, A6, A7]

pub let reg_caller_saved_list : Array[Reg] = reg_temp_list + reg_arg_list

pub let reg_callee_saved_list : Array[Reg] = reg_saved_list

pub let reg_allocatable_list : Array[Reg] = reg_callee_saved_list +
  reg_caller_saved_list

pub let reg_fp : Reg = Fp

pub let reg_closure : Reg = S10

pub let reg_alloc : Reg = S11

pub let reg_ret : Reg = A0

pub let reg_swap : Reg = T6

pub fn Reg::to_string(self : Reg) -> String {
  match self {
    Zero => "zero"
    Ra => "ra"
    Sp => "sp"
    Gp => "gp"
    Tp => "tp"
    T0 => "t0"
    T1 => "t1"
    T2 => "t2"
    Fp => "s0"
    S1 => "s1"
    A0 => "a0"
    A1 => "a1"
    A2 => "a2"
    A3 => "a3"
    A4 => "a4"
    A5 => "a5"
    A6 => "a6"
    A7 => "a7"
    S2 => "s2"
    S3 => "s3"
    S4 => "s4"
    S5 => "s5"
    S6 => "s6"
    S7 => "s7"
    S8 => "s8"
    S9 => "s9"
    S10 => "s10"
    S11 => "s11"
    T3 => "t3"
    T4 => "t4"
    T5 => "t5"
    T6 => "t6"
    V(r) => "v\{r.reg}"
  }
}

impl Show for Reg with output(self, logger : Logger) {
  logger.write_string(self.to_string())
}

/// RISC-V floating point register
pub enum FReg {
  Ft0
  Ft1
  Ft2
  Ft3
  Ft4
  Ft5
  Ft6
  Ft7
  Fs0
  Fs1
  Fa0
  Fa1
  Fa2
  Fa3
  Fa4
  Fa5
  Fa6
  Fa7
  Fs2
  Fs3
  Fs4
  Fs5
  Fs6
  Fs7
  Fs8
  Fs9
  Fs10
  Fs11
  Ft8
  Ft9
  Ft10
  Ft11
  Fv(AnyReg)
} derive(Eq, Compare, Hash)

pub fn phy_to_virt_f(self : FReg) -> AnyReg {
  match self {
    Fv(r) => r
    Ft0 => any_freg(-33)
    Ft1 => any_freg(-34)
    Ft2 => any_freg(-35)
    Ft3 => any_freg(-36)
    Ft4 => any_freg(-37)
    Ft5 => any_freg(-38)
    Ft6 => any_freg(-39)
    Ft7 => any_freg(-40)
    Fs0 => any_freg(-41)
    Fs1 => any_freg(-42)
    Fa0 => any_freg(-43)
    Fa1 => any_freg(-44)
    Fa2 => any_freg(-45)
    Fa3 => any_freg(-46)
    Fa4 => any_freg(-47)
    Fa5 => any_freg(-48)
    Fa6 => any_freg(-49)
    Fa7 => any_freg(-50)
    Fs2 => any_freg(-51)
    Fs3 => any_freg(-52)
    Fs4 => any_freg(-53)
    Fs5 => any_freg(-54)
    Fs6 => any_freg(-55)
    Fs7 => any_freg(-56)
    Fs8 => any_freg(-57)
    Fs9 => any_freg(-58)
    Fs10 => any_freg(-59)
    Fs11 => any_freg(-60)
    Ft8 => any_freg(-61)
    Ft9 => any_freg(-62)
    Ft10 => any_freg(-63)
    Ft11 => any_freg(-64)
  }
}

pub fn virt_f_to_phy(self : AnyReg) -> FReg {
  let r = match self.reg {
    -33 => Ft0
    -34 => Ft1
    -35 => Ft2
    -36 => Ft3
    -37 => Ft4
    -38 => Ft5
    -39 => Ft6
    -40 => Ft7
    -41 => Fs0
    -42 => Fs1
    -43 => Fa0
    -44 => Fa1
    -45 => Fa2
    -46 => Fa3
    -47 => Fa4
    -48 => Fa5
    -49 => Fa6
    -50 => Fa7
    -51 => Fs2
    -52 => Fs3
    -53 => Fs4
    -54 => Fs5
    -55 => Fs6
    -56 => Fs7
    -57 => Fs8
    -58 => Fs9
    -59 => Fs10
    -60 => Fs11
    -61 => Ft8
    -62 => Ft9
    -63 => Ft10
    -64 => Ft11
    _ =>
      if self.reg >= 0 && self.class == F {
        Fv(self)
      } else {
        abort("invalid register \{self} to float register")
      }
  }
  r
}

pub let freg_temp_list : Array[FReg] = [
  Ft0,
  Ft1,
  Ft2,
  Ft3,
  Ft4,
  Ft5,
  Ft6,
  Ft7,
  Ft8,
  Ft9,
  Ft10,
]

pub let freg_saved_list : Array[FReg] = [
  Fs0,
  Fs1,
  Fs2,
  Fs3,
  Fs4,
  Fs5,
  Fs6,
  Fs7,
  Fs8,
  Fs9,
  Fs10,
  Fs11,
]

pub let freg_arg_list : Array[FReg] = [Fa0, Fa1, Fa2, Fa3, Fa4, Fa5, Fa6, Fa7]

pub let freg_caller_saved_list : Array[FReg] = freg_temp_list + freg_arg_list

pub let freg_callee_saved_list : Array[FReg] = freg_saved_list

pub let freg_allocatable_list : Array[FReg] = freg_callee_saved_list +
  freg_caller_saved_list

pub let freg_ret : FReg = Fa0

pub let freg_swap : FReg = Ft11

pub fn FReg::to_string(self : FReg) -> String {
  match self {
    Ft0 => "ft0"
    Ft1 => "ft1"
    Ft2 => "ft2"
    Ft3 => "ft3"
    Ft4 => "ft4"
    Ft5 => "ft5"
    Ft6 => "ft6"
    Ft7 => "ft7"
    Fs0 => "fs0"
    Fs1 => "fs1"
    Fa0 => "fa0"
    Fa1 => "fa1"
    Fa2 => "fa2"
    Fa3 => "fa3"
    Fa4 => "fa4"
    Fa5 => "fa5"
    Fa6 => "fa6"
    Fa7 => "fa7"
    Fs2 => "fs2"
    Fs3 => "fs3"
    Fs4 => "fs4"
    Fs5 => "fs5"
    Fs6 => "fs6"
    Fs7 => "fs7"
    Fs8 => "fs8"
    Fs9 => "fs9"
    Fs10 => "fs10"
    Fs11 => "fs11"
    Ft8 => "ft8"
    Ft9 => "ft9"
    Ft10 => "ft10"
    Ft11 => "ft11"
    Fv(r) => "fv\{r.reg}"
  }
}

impl Show for FReg with output(self, logger : Logger) {
  logger.write_string(self.to_string())
}
