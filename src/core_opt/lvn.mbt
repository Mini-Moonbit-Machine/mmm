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
        let old_shapes = shape.to_array()
        match insn {
          @core.Insn::Call(..) | ExtCall(..) | Apply(..) =>
            // keep value across calls may use extra instructions to save/restore
            // shape.clear()
            ()
          _ => ()
        }
        for s in old_shapes {
          match s.0 {
            @core.Insn::Load(..) => shape.remove(s.0)
            _ =>
              if s.0.effectful() {
                self.tracer.abort("LVN: effectful insn in shape")
              }
          }
        }
        continue
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
        for block in self.func.blocks {
          let (_, block) = block
          orig.replace_all_uses_with(
            self.func,
            block.bb_or_header_bb(self.func),
            subst,
          )
        }
      }
      None => changed = false
    }
  }
}
