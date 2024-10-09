typealias AnyReg = @riscv.AnyReg

typealias Set[K] = @hashset.T[K]

pub struct BlockInfo {
  defs : Set[AnyReg] // defined in the block
  uses : Set[AnyReg] // used in the block
  upward_exposed : Set[AnyReg] // used but not defined in the block
  params_defs : Set[AnyReg] // block parameters defined
}

pub fn BlockInfo::to_string(self : BlockInfo) -> String {
  let defs = self.defs.iter().to_string()
  let uses = self.uses.iter().to_string()
  let upward_exposed = self.upward_exposed.iter().to_string()
  let params_defs = self.params_defs.iter().to_string()
  "defs: \{defs}, uses: \{uses}, upward_exposed: \{upward_exposed}, params_defs: \{params_defs}"
}

let call_def : Array[AnyReg] = @riscv.reg_caller_saved_list.map(
    fn(r) { r.to_any() },
  ) +
  @riscv.freg_caller_saved_list.map(fn(r) { r.to_any() })

let xret_use : Array[AnyReg] = [@riscv.reg_ret.to_any()]

let fret_use : Array[AnyReg] = [@riscv.freg_ret.to_any()]

fn call_use_precise(num_xregs : Int, num_fregs : Int) -> Array[AnyReg] {
  let fregs = @riscv.freg_arg_list[0:num_fregs].iter().map(fn(r) { r.to_any() })
  let xregs = @riscv.reg_arg_list[0:num_xregs].iter().map(fn(r) { r.to_any() })
  fregs.to_array() + xregs.to_array()
}

pub fn def_use(insn : MInsn) -> (Array[AnyReg], Array[AnyReg]) {
  match insn {
    Add(~rd, ~rs1, ~rs2)
    | Sub(~rd, ~rs1, ~rs2)
    | Xor(~rd, ~rs1, ~rs2)
    | Or(~rd, ~rs1, ~rs2)
    | And(~rd, ~rs1, ~rs2)
    | Sll(~rd, ~rs1, ~rs2)
    | Srl(~rd, ~rs1, ~rs2)
    | Sra(~rd, ~rs1, ~rs2) | Slt(~rd, ~rs1, ~rs2) | Sltu(~rd, ~rs1, ~rs2) =>
      ([rd.to_any()], [rs1.to_any(), rs2.to_any()])
    Addi(~rd, ~rs1, ..)
    | Xori(~rd, ~rs1, ..)
    | Ori(~rd, ~rs1, ..)
    | Andi(~rd, ~rs1, ..)
    | Slli(~rd, ~rs1, ..)
    | Srli(~rd, ~rs1, ..)
    | Srai(~rd, ~rs1, ..) | Slti(~rd, ~rs1, ..) | Sltiu(~rd, ~rs1, ..) =>
      ([rd.to_any()], [rs1.to_any()])
    Lbu(~rd, ~mem)
    | Lhu(~rd, ~mem)
    | Lwu(~rd, ~mem)
    | Lb(~rd, ~mem) | Lh(~rd, ~mem) | Lw(~rd, ~mem) | Ld(~rd, ~mem) =>
      ([rd.to_any()], [mem.base.to_any()])
    Sb(~rs, ~mem) | Sh(~rs, ~mem) | Sw(~rs, ~mem) | Sd(~rs, ~mem) =>
      ([], [rs.to_any(), mem.base.to_any()])
    Beq(~rs1, ~rs2, ..)
    | Bne(~rs1, ~rs2, ..)
    | Blt(~rs1, ~rs2, ..)
    | Bge(~rs1, ~rs2, ..)
    | Ble(~rs1, ~rs2, ..)
    | Bgt(~rs1, ~rs2, ..) | Bltu(~rs1, ~rs2, ..) | Bgeu(~rs1, ~rs2, ..) =>
      ([], [rs1.to_any(), rs2.to_any()])
    Ecall => ([], [])
    Mul(~rd, ~rs1, ~rs2)
    | Mulw(~rd, ~rs1, ~rs2)
    | Mulh(~rd, ~rs1, ~rs2)
    | Mulhsu(~rd, ~rs1, ~rs2)
    | Mulhu(~rd, ~rs1, ~rs2)
    | Div(~rd, ~rs1, ~rs2)
    | Divw(~rd, ~rs1, ~rs2)
    | Divu(~rd, ~rs1, ~rs2)
    | Rem(~rd, ~rs1, ~rs2) | Remw(~rd, ~rs1, ~rs2) | Remu(~rd, ~rs1, ~rs2) =>
      ([rd.to_any()], [rs1.to_any(), rs2.to_any()])
    FaddD(~rd, ~rs1, ~rs2)
    | FsubD(~rd, ~rs1, ~rs2) | FmulD(~rd, ~rs1, ~rs2) | FdivD(~rd, ~rs1, ~rs2) =>
      ([rd.to_any()], [rs1.to_any(), rs2.to_any()])
    Fld(~rd, ~mem) => ([rd.to_any()], [mem.base.to_any()])
    Fsd(~rs, ~mem) => ([], [rs.to_any(), mem.base.to_any()])
    FeqD(~rd, ~rs1, ~rs2) | FleD(~rd, ~rs1, ~rs2) =>
      ([rd.to_any()], [rs1.to_any(), rs2.to_any()])
    FmvDX(~rd, ~rs) => ([rd.to_any()], [rs.to_any()])
    FcvtWD(~rd, ~rs, ..) => ([rd.to_any()], [rs.to_any()])
    FcvtDW(~rd, ~rs) => ([rd.to_any()], [rs.to_any()])
    FabsD(~rd, ~rs) | FsqrtD(~rd, ~rs) => ([rd.to_any()], [rs.to_any()])
    La(~rd, ..) | Li(~rd, ..) => ([rd.to_any()], [])
    Neg(~rd, ~rs) | Mv(~rd, ~rs) => ([rd.to_any()], [rs.to_any()])
    FnegD(~rd, ~rs) | FmvD(~rd, ~rs) => ([rd.to_any()], [rs.to_any()])
    XRet => ([], xret_use)
    FRet => ([], fret_use)
    Call(~num_xregs, ~num_fregs, ..) =>
      (call_def, call_use_precise(num_xregs, num_fregs))
    Jalr(~num_xregs, ~num_fregs, ~target) => {
      let uses = call_use_precise(num_xregs, num_fregs)
      uses.push(target.to_any())
      (call_def, uses)
    }
    SaveCtx1 | RestoreCtx1 => ([], [])
    SaveCtx2 | RestoreCtx2 => ([], [])
    Nop | J(..) | Jr(..) | Tail(..) | Label(_) | Comment(_) => ([], [])
  }
}

pub fn BlockInfo::compute(mbb : MBasicBlock) -> BlockInfo {
  let defs = Set::new()
  let uses = Set::new()
  let upward_exposed = Set::new()
  let params_defs = Set::new()
  for param in mbb.params {
    params_defs.insert(param)
  }
  // still SSA? no kills
  for insn in mbb.insns {
    let (d, u) = def_use(insn)
    for reg in d {
      defs.insert(reg)
    }
    for reg in u {
      uses.insert(reg)
      if defs.contains(reg) == false {
        upward_exposed.insert(reg)
      }
    }
  }
  for control in mbb.control {
    let (d, u) = def_use(control)
    for reg in d {
      defs.insert(reg)
    }
    for reg in u {
      uses.insert(reg)
      if defs.contains(reg) == false {
        upward_exposed.insert(reg)
      }
    }
  }
  BlockInfo::{ defs, uses, upward_exposed, params_defs }
}

pub struct IteratedBlockInfo {
  live_in : Set[AnyReg]
  live_out : Set[AnyReg]
  block_info : BlockInfo
  defs : Array[Array[AnyReg]]
  live_set : Array[Array[AnyReg]]
}

pub fn IteratedBlockInfo::to_string(self : IteratedBlockInfo) -> String {
  let live_in = self.live_in.iter().to_string()
  let live_out = self.live_out.iter().to_string()
  "live_in: \{live_in}, live_out: \{live_out}"
}

pub fn IteratedBlockInfo::compute_live_set(
  self : IteratedBlockInfo,
  block : MBasicBlock
) -> Unit {
  let live_set = self.live_set
  let defs = self.defs
  let live = MutSet::from_iter(self.live_out.iter())
  for insn in block.control.rev() {
    live_set.push(live.iter().to_array())
    let (d, u) = def_use(insn)
    defs.push(d)
    d.each(fn(reg) { live.remove(reg) })
    u.each(fn(reg) { live.insert(reg) })
    live.remove(@riscv.Sp.to_any())
  }
  for insn in block.insns.rev() {
    live_set.push(live.iter().to_array())
    let (d, u) = def_use(insn)
    defs.push(d)
    d.each(fn(reg) { live.remove(reg) })
    u.each(fn(reg) { live.insert(reg) })
    live.remove(@riscv.Sp.to_any())
  }
  live_set.push(live.iter().to_array())
  if live.size() != self.live_in.size() {
    abort("\{block.id._} live set mismatch: \{live} and \{self.live_in}")
  }
  defs.rev_inplace()
  live_set.rev_inplace()
}

pub fn IteratedBlockInfo::compute_live_in_out(
  self : IteratedBlockInfo,
  succ_info : Array[IteratedBlockInfo]
) -> Bool {
  let live_in = self.live_in
  let live_out = self.live_out
  let info = self.block_info
  let old_live_in_size = live_in.size()
  let old_live_out_size = live_out.size()

  // Update LiveIn =
  // ParamsDefs(B)
  info.params_defs.each(fn(reg) { live_in.insert(reg) })
  // UpwardExposed(B)
  info.upward_exposed.each(fn(reg) { live_in.insert(reg) })
  // LiveOut(B) \ Defs(B)
  live_out.each(
    fn(reg) { if info.defs.contains(reg) == false { live_in.insert(reg) } },
  )

  // Update LiveOut =
  // LiveIn(S) for each successor S
  for succ in succ_info {
    succ.live_in.each(fn(reg) { live_out.insert(reg) })
  }
  live_in.size() != old_live_in_size || live_out.size() != old_live_out_size
}

pub struct Liveness {
  func : @core.Fn?
  mfunc : MFn
  block_info : MutMap[String, BlockInfo]
  iter_block_info : MutMap[String, IteratedBlockInfo]
}

pub fn Liveness::new(func : @core.Fn?, mfunc : MFn) -> Liveness {
  let block_info = MutMap::new()
  let iter_block_info = MutMap::new()
  Liveness::{ func, mfunc, iter_block_info, block_info }
}

pub fn Liveness::compute_block_liveness(self : Liveness) -> Unit {
  for mbb in self.mfunc.blocks {
    let (lbl, mbb) = mbb
    let info = BlockInfo::compute(mbb)
    let live_set = []
    let defs = []
    self.block_info[lbl._] = info
    self.iter_block_info[lbl._] = {
      live_in: Set::new(),
      live_out: Set::new(),
      block_info: info,
      live_set,
      defs,
    }
  }
  match self.func {
    None =>
      for ibi in self.iter_block_info {
        let (_, ibi) = ibi
        let mut changed = true
        while changed {
          changed = ibi.compute_live_in_out([])
        }
      }
    Some(func) => {
      // PO doesn't work perfect when encountering loops
      let order = @core_analysis.Ordering::dfs(func)
        .postorder()
        .to_basic_blocks()
      let mut changed = true
      while changed {
        changed = false
        for bb in order._ {
          let cur_info = self.iter_block_info[fn_block_name(func, bb.id)].unwrap()
          let succs = []
          for succ in bb.control.successors() {
            let succ_info = self.iter_block_info[fn_block_name(func, succ)].unwrap()
            succs.push(succ_info)
          }
          changed = cur_info.compute_live_in_out(succs) || changed
        }
      }
    }
  }
  for ibi in self.iter_block_info {
    let (name, ibi) = ibi
    ibi.compute_live_set(self.mfunc.blocks[name].unwrap())
  }
}
