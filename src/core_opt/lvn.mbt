pub struct LocalValueNumbering {
  core : @core.Core
  tracer : @util.SubTracer
}

pub fn LocalValueNumbering::new(
  core : @core.Core,
  tracer : @util.SubTracer
) -> LocalValueNumbering {
  { core, tracer }
}

pub fn LocalValueNumbering::go(self : LocalValueNumbering) -> Unit {
  for func in self.core.funcs {
    let (_, func) = func
    for block in func.blocks {
      let (_, block) = block
      if block.is_basic_block() == false {
        continue
      }
      let bb = block.bb_or_header_bb(func)
      let numbering = Numbering::new(func, bb, self.tracer)
      numbering.go()
    }
  }
}

pub struct Numbering {
  func : @core.Fn
  bb : @core.BasicBlock
  tracer : @util.SubTracer
}

pub fn Numbering::new(
  func : @core.Fn,
  bb : @core.BasicBlock,
  tracer : @util.SubTracer
) -> Numbering {
  { func, bb, tracer }
}

pub fn Numbering::go(self : Numbering) -> Unit {
  let bb = self.bb
  let mut changed = true
  while changed {
    let mut subst = None
    let counter = Ref::new(0)
    let vnum = {}
    let numv = {}
    let shape = {}
    for i, iref in bb.seq {
      let insn = self.func.insns.get(iref).unwrap()
      if insn.effectful() {
        vnum.clear()
        numv.clear()
        shape.clear()
      }
      let v_of_insn = @core.Value::Insn(~iref)
      match shape.get(insn) {
        Some(v) => {
          vnum[v_of_insn] = v
          subst = Some((v_of_insn, numv[v].unwrap(), i, iref))
          break
        }
        None => {
          let n = counter.val
          counter.val = n + 1
          shape[insn] = n
          vnum[v_of_insn] = n
          numv[n] = v_of_insn
        }
      }
    }
    match subst {
      Some((orig, subst, i, iref)) => {
        changed = true
        self.tracer.println("LVN subst \{orig} with \{subst}")
        bb.seq.remove(i) |> ignore
        self.func.insns.remove(iref)
        orig.replace_all_uses_with(self.func, bb, subst)
      }
      None => changed = false
    }
  }
}
