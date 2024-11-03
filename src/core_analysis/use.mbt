pub struct InsnUseAnalysis {
  func : @core.Fn
  tracer : @util.SubTracer
}

pub fn InsnUseAnalysis::new(
  func : @core.Fn,
  tracer : @util.SubTracer
) -> InsnUseAnalysis {
  { func, tracer }
}

pub fn InsnUseAnalysis::go(self : InsnUseAnalysis) -> Map[@core.InsnRef, Int] {
  let func = self.func
  let uses = {}
  fn add_use(val : @core.Value) -> Unit {
    match val {
      @core.Value::Insn(~iref) => uses[iref] = uses[iref].or(0) + 1
      _ => ()
    }
  }

  for block in func.blocks {
    let (_, block) = block
    if block.is_basic_block() == false {
      continue
    }
    let bb = block.bb_or_header_bb(func)
    for iref in bb.seq {
      let insn = func.insns.get(iref).unwrap()
      insn.replace(
        fn(val) {
          add_use(val)
          val
        },
      )
      |> ignore
    }
    bb.control.replace(
      fn(val) {
        add_use(val)
        val
      },
    )
    |> ignore
  }
  func.mems
  .iter()
  .each(
    fn(mem) {
      let (_, mem) = mem
      mem.replace(
        fn(val) {
          add_use(val)
          val
        },
      )
      |> ignore
    },
  )
  uses
}
