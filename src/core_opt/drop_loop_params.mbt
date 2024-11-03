pub struct DropLoopParams {
  core : @core.Core
  tracer : @util.SubTracer
}

pub struct FnDropLoopParams {
  func : @core.Fn
  tracer : @util.SubTracer
}

pub fn DropLoopParams::new(
  core : @core.Core,
  tracer : @util.SubTracer
) -> DropLoopParams {
  { core, tracer }
}

pub fn DropLoopParams::go(self : DropLoopParams) -> Unit {
  for func in self.core.funcs {
    let (_, func) = func
    let dlp = FnDropLoopParams::new(func, self.tracer)
    dlp.go()
  }
}

pub fn FnDropLoopParams::new(
  func : @core.Fn,
  tracer : @util.SubTracer
) -> FnDropLoopParams {
  { func, tracer }
}

pub fn FnDropLoopParams::go(self : FnDropLoopParams) -> Unit {
  for block in self.func.blocks {
    let (_, block) = block
    guard let @core.Block::Loop(~l) = block else { _ => continue }
    let header = l.header
    let header_bb = self.func.blocks
      .get(header)
      .unwrap()
      .bb_or_header_bb(self.func)
    let loop_insns = block
      .all_basic_blocks(self.func)
      .iter()
      .flat_map(fn(block) { block.seq.iter() })
    let loop_insns = @hashset.from_iter(loop_insns)
    let drop_list = []
    fn can_be_dropped(val : @core.Value, bb : @core.BlockRef, i : Int) {
      match val {
        @core.Value::Unit | Bool(..) | Int32(..) | Int64(..) | Double(..) =>
          true
        BlockParam(~bref, ~bpref) => bref == bb && bpref._.id == i
        FnParam(..) => true
        FreeVar(..) => true
        Insn(~iref) => loop_insns.contains(iref) == false
        _ => false
      }
    }

    for i in 0..<header_bb.params.length() {
      let bp = @core.block_param(header, i)
      let defs = @core_analysis.find_all_defs_of_value(self.func, bp)
      if defs.iter().all(fn(val) { can_be_dropped(val, header, i) }) {
        drop_list.push(bp)
      }
    }
    self.tracer.println("Dropping loop params: \{drop_list}")
  }
}
