pub struct FloatOffset {
  core : @core.Core
  tracer : @util.SubTracer
}

pub struct BasicBlockFloatOffset {
  func : @core.Fn
  bb : @core.BasicBlock
  mut insn_counter : Int
  tracer : @util.SubTracer
}

pub fn FloatOffset::new(
  core : @core.Core,
  tracer : @util.SubTracer
) -> FloatOffset {
  { core, tracer }
}

pub fn FloatOffset::go(self : FloatOffset) -> Unit {
  for func in self.core.funcs {
    let (_, func) = func
    for block in func.blocks {
      let (_, block) = block
      if block.is_basic_block() == false {
        continue
      }
      let bb = block.bb_or_header_bb(func)
      let numbering = BasicBlockFloatOffset::new(func, bb, self.tracer)
      numbering.go()
    }
  }
}

pub fn BasicBlockFloatOffset::new(
  func : @core.Fn,
  bb : @core.BasicBlock,
  tracer : @util.SubTracer
) -> BasicBlockFloatOffset {
  let counter = FnCounter::new_from_existing(func)
  { func, bb, tracer, insn_counter: counter.insn_counter }
}

pub fn BasicBlockFloatOffset::go(self : BasicBlockFloatOffset) -> Unit {
  let bb = self.bb
  let mut changed = true
  fn next_iref() -> @core.InsnRef {
    let counter = self.insn_counter
    self.insn_counter += 1
    { id: counter }
  }

  while changed {
    let mut floated = None
    for i, iref in bb.seq {
      let insn = self.func.insns.get(iref).unwrap()
      fn is_target_mem_address(ma : @core.Address) {
        match ma {
          @core.Address::Offset(~val, ~offset, ~ty, ~width) => {
            let iref = next_iref()
            Some(
              (
                iref,
                @core.Address::Offset(
                  val=@core.Value::Int64(val=0),
                  ~offset,
                  ~ty,
                  ~width,
                ),
                @core.Address::Raw(
                  ~val,
                  ~ty,
                  raw_offset=@core.Value::Insn(~iref),
                ),
              ),
            )
          }
          _ => None
        }
      }

      match insn {
        Store(~ty, ~ma, ~val) => {
          let ma = is_target_mem_address(ma)
          match ma {
            Some((iref, new_ma1, new_ma2)) => {
              let new_addr_insn = @core.ComputeAddress(ma=new_ma1)
              let new_insn = @core.Insn::Store(~ty, ma=new_ma2, ~val)
              floated = Some((iref, new_insn, new_addr_insn, i))
            }
            None => ()
          }
        }
        Load(~ty, ~ma) => {
          let ma = is_target_mem_address(ma)
          match ma {
            Some((new_iref, new_ma1, new_ma2)) => {
              let new_addr_insn = @core.ComputeAddress(ma=new_ma1)
              let new_insn = @core.Insn::Load(~ty, ma=new_ma2)
              floated = Some((new_iref, new_insn, new_addr_insn, i))
            }
            None => ()
          }
        }
        _ => ()
      }
    }
    match floated {
      Some((addr_calc_iref, new_insn, new_addr_calc_insn, idx)) => {
        self.func.insns.set(addr_calc_iref, new_addr_calc_insn)
        self.func.insns.set(bb.seq[idx], new_insn)
        bb.seq.insert(idx, addr_calc_iref)
      }
      None => changed = false
    }
  }
}
