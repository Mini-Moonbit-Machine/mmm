pub struct Inliner {
  core : @core.Core
  fcf : @core_analysis.FnCallFreq
  tracer : @util.SubTracer
}

pub fn Inliner::new(
  core : @core.Core,
  fcf : @core_analysis.FnCallFreq,
  tracer : @util.SubTracer
) -> Inliner {
  { core, fcf, tracer }
}

pub fn Inliner::go(self : Inliner) -> Unit {
  for func in self.core.funcs {
    let (_, func) = func
    let inliner = FnInliner::new(self.core, func, self.fcf, self.tracer)
    inliner.split()
  }
}

pub struct FnInliner {
  core : @core.Core
  func : @core.Fn
  fcf : @core_analysis.FnCallFreq
  tracer : @util.SubTracer
}

pub fn FnInliner::new(
  core : @core.Core,
  func : @core.Fn,
  fcf : @core_analysis.FnCallFreq,
  tracer : @util.SubTracer
) -> FnInliner {
  { core, func, fcf, tracer }
}

pub fn FnInliner::split_bb_at_call(
  self : FnInliner,
  bb : @core.BasicBlock,
  call_iref : @core.InsnRef,
  call_idx : Int
) -> @core.BlockRef? {
  let id = self.func.next_block_id()
  guard let @core.Insn::Call(~ty, ~f, ..) = self.func.insns
    .get(bb.seq[call_idx])
    .unwrap() else {
    _ => self.tracer.abort("expected call instruction")
  }
  guard self.fcf._.get(f).unwrap() == @core_analysis.CallFrequnce::Once else {
    self.tracer.abort("expected call frequency to be Once")
  }
  if call_idx == bb.seq.length() - 1 {
    return None
  }
  let new_seq = bb.seq.drain(call_idx + 1, bb.seq.length())
  let post_bb = @core.BasicBlock::{
    id,
    seq: new_seq,
    control: bb.control,
    params: [ty],
  }
  let call_value = @core.Value::Insn(iref=call_iref)
  let block_param_0 = @core.Value::BlockParam(
    bref=post_bb.id,
    bpref=@core.BlockParamRef(@core.Id::new(0)),
  )
  bb.control = @core.Control::Jump(target=id, args=[call_value])
  self.func.blocks.set(id, @core.Block::BasicBlock(bb=post_bb))
  for block in self.func.blocks {
    let (bref, block) = block
    if not(block.is_basic_block()) || bref == bb.id {
      continue
    }
    let bb = block.bb_or_header_bb(self.func)
    call_value.replace_all_uses_with(self.func, bb, block_param_0)
  }
  Some(id)
}

fn FnInliner::split(self : FnInliner) -> Unit {
  let split_mem = @hashset.new()
  fn do(block : @core.Block) {
    let mut changed = false
    let bb = block.bb_or_header_bb(self.func)
    for idx, iref in bb.seq {
      let insn = self.func.insns.get(iref).unwrap()
      match insn {
        Call(~f, ..) =>
          if self.fcf._.get(f).unwrap() == @core_analysis.CallFrequnce::Once &&
            not(split_mem.contains(bb.id)) {
            split_mem.insert(bb.id)
            let r = self.split_bb_at_call(bb, iref, idx)
            if not(r.is_empty()) {
              // we have modified self.func.blocks
              // so we need to break and restart the loop
              changed = true
              break
            }
          }
        _ => ()
      }
    }
    changed
  }

  let mut changed = true
  while changed {
    changed = false
    for block in self.func.blocks {
      let (_, block) = block
      if not(block.is_basic_block()) {
        continue
      }
      changed = do(block) || changed
      if changed {
        break
      }
    }
  }
}
