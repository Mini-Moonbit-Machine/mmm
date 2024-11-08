struct LICM {
  core : @core.Core
  tracer : @util.SubTracer
}

fn LICM::new(core : @core.Core, tracer : @util.SubTracer) -> LICM {
  { core, tracer }
}

fn LICM::go(self : LICM) -> Unit {
  for func in self.core.funcs {
    let (_, func) = func
    let licm = FnLICM::new(func, self.tracer)
    licm.go()
  }
}

struct FnLICM {
  func : @core.Fn
  tracer : @util.SubTracer
}

fn FnLICM::new(func : @core.Fn, tracer : @util.SubTracer) -> FnLICM {
  { func, tracer }
}

fn FnLICM::handle_loop(self : FnLICM, l : @core.Loop) -> Unit {
  match l.pre_header {
    Some(pre) => {
      let pre = self.func.blocks
        .get(pre)
        .or_else(fn() { self.tracer.abort("pre_header not found") })
        .bb_or_header_bb(self.func)
      for bref in l.blocks {
        let bb = self.func.blocks
          .get(bref)
          .or_else(fn() { self.tracer.abort("block not found") })
        if bb.is_basic_block() == false {
          continue
        }
        self.handle_block(bb.bb_or_header_bb(self.func), pre)
      }
    }
    _ => ()
  }
}

fn FnLICM::handle_block(
  self : FnLICM,
  bb : @core.BasicBlock,
  pre_header : @core.BasicBlock
) -> Unit {
  let mut changed = true
  while changed {
    let mut lifted = None
    for i, iref in bb.seq {
      let insn = self.func.insns.get(iref).unwrap()
      match insn {
        LoadSymbol(_) => {
          changed = true
          lifted = Some((iref, i))
        }
        _ => ()
      }
    }
    match lifted {
      Some((iref, i)) => {
        bb.seq.remove(i) |> ignore
        pre_header.seq.push(iref)
      }
      None => changed = false
    }
  }
}

fn FnLICM::go(self : FnLICM) -> Unit {
  for block in self.func.blocks {
    let (_, block) = block
    guard let @core.Block::Loop(~l) = block else { _ => continue }
    self.handle_loop(l)
  }
}
