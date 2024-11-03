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
  let inliners = []
  for func in self.core.funcs {
    let (_, func) = func
    let inliner = FnInliner::new(self.core, func, self.fcf, self.tracer)
    inliners.push(inliner)
  }
  let s = []
  for inliner in inliners {
    let splitted = inliner.split()
    s.push(splitted)
  }
  self.tracer.record("core_opt_after_splitted", self.core.to_pretty_print())
  for i, inliner in inliners {
    let splitted = s[i]
    inliner.inline(splitted)
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
  guard self.fcf._.get(f).unwrap() == @core_analysis.CallFrequncy::Once else {
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
  let block_param_0 = @core.block_param(post_bb.id, 0)
  bb.control = @core.Control::Jump(target=id, args=[call_value])
  self.func.blocks.set(id, @core.Block::BasicBlock(bb=post_bb))
  for block in self.func.blocks {
    let (bref, block) = block
    if not(block.is_basic_block()) || bref == bb.id {
      continue
    }
    let bb = block.bb_or_header_bb(self.func)
    self.tracer.println("post splitting: \{call_value} -> \{block_param_0}")
    call_value.replace_all_uses_with(self.func, bb, block_param_0)
  }
  Some(id)
}

fn allow_inline(
  self : FnInliner,
  call_iref : @core.InsnRef,
  f : @core.FnRef,
  memo : @hashset.T[@core.InsnRef]
) -> Bool {
  self.fcf._.get(f).unwrap() == @core_analysis.CallFrequncy::Once &&
  not(memo.contains(call_iref)) &&
  self.func.name.contains("main") == false
}

fn FnInliner::split(self : FnInliner) -> @hashset.T[@core.InsnRef] {
  let split_mem = @hashset.new()
  let splitted = @hashset.new()
  fn do(block : @core.Block) {
    let mut changed = false
    let bb = block.bb_or_header_bb(self.func)
    for idx, iref in bb.seq {
      let insn = self.func.insns.get(iref).unwrap()
      match insn {
        Call(~f, ..) =>
          if self.allow_inline(iref, f, split_mem) {
            split_mem.insert(iref)
            let r = self.split_bb_at_call(bb, iref, idx)
            match r {
              Some(_) => {
                // we have modified self.func.blocks
                // so we need to break and restart the loop
                changed = true
                splitted.insert(iref)
                break
              }
              None => ()
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
  splitted
}

fn FnInliner::fix_return_to_jump(
  self : FnInliner,
  callee : @core.Fn,
  ~kont : @core.BlockRef
) -> Unit {
  for callee_block in callee.blocks {
    let (_, callee_block) = callee_block
    if not(callee_block.is_basic_block()) {
      continue
    }
    let bb = callee_block.bb_or_header_bb(callee)
    match bb.control {
      @core.Control::Return(~args) => {
        if args.length() != 1 {
          self.tracer.abort("expected one return argument")
        }
        bb.control = @core.Control::Jump(target=kont, ~args)
      }
      _ => ()
    }
  }
}

fn FnInliner::fix_entry(self : FnInliner, callee : @core.Fn) -> Unit {
  let entry = callee.blocks.get(callee.entry).unwrap()
  let entry = entry.bb_or_header_bb(callee)
  entry.params = match callee.ty {
    @core.Type::DirectFn(~params, ..) => params
    _ => self.tracer.abort("expected direct function type")
  }
}

fn FnInliner::fix_fn_parameters_and_mems(
  self : FnInliner,
  callee : @core.Fn,
  args : Array[@core.Value]
) -> Unit {
  for callee_block in callee.blocks {
    let (_, callee_block) = callee_block
    if not(callee_block.is_basic_block()) {
      continue
    }
    let bb = callee_block.bb_or_header_bb(callee)
    @core.replace_all(
      callee,
      bb,
      map=fn(v) {
        match v {
          @core.Value::Mem(~mref, ..) => Mem(fref=self.func.id, ~mref)
          @core.Value::FnParam(~fpref, ..) => {
            let n = fpref.id
            if n >= args.length() {
              self.tracer.abort(
                "expected parameter index to be less than the length of params",
              )
            }
            args[n]
          }
          _ => v
        }
      },
    )
  }
}

fn FnInliner::replace_call(
  self : FnInliner,
  bb : @core.BasicBlock,
  call_insn : @core.Insn
) -> Unit {
  guard let Call(~f, args=call_args, ..) = call_insn else {
    _ => self.tracer.abort("expected call instruction")
  }
  let callee = self.core.funcs.get(f).unwrap()
  let counter = FnCounter::new_from_existing(self.func)
  let renamer = Renamer::new(callee, counter, self.tracer)
  let callee = renamer.shift()
  self.tracer.println("shifted callee \{callee.to_pretty_print()}")
  if callee.fvars.length() > 0 {
    self.tracer.abort("expected no free variables in inlined function")
  }
  // call has been ensured that it's the last instruction in the block and there must be a jump
  guard let Jump(~target, ~args) = bb.control else {
    _ => self.tracer.abort("expected jump instruction")
  }
  if args.length() != 1 {
    self.tracer.abort("expected one argument in jump instruction")
  }
  bb.seq.pop() |> ignore // remove the call instruction
  bb.control = @core.Control::Jump(target=callee.entry, args=[])
  // self.fix_entry(callee)
  self.fix_return_to_jump(callee, kont=target)
  self.fix_fn_parameters_and_mems(callee, call_args)
  callee.blocks.each(fn(bref, blk) { self.func.blocks.set(bref, blk) })
  callee.mems.each(fn(mem, mem_ty) { self.func.mems.set(mem, mem_ty) })
  callee.insns.each(fn(iref, insn) { self.func.insns.set(iref, insn) })
}

fn FnInliner::inline(
  self : FnInliner,
  splitted : @hashset.T[@core.InsnRef]
) -> Unit {
  let inline_mem = @hashset.new()
  fn do(block : @core.Block) {
    let mut changed = false
    let bb = block.bb_or_header_bb(self.func)
    for iref in bb.seq {
      let insn = self.func.insns.get(iref).unwrap()
      match insn {
        Call(~f, ..) =>
          if self.allow_inline(iref, f, inline_mem) && splitted.contains(iref) {
            inline_mem.insert(iref)
            self.tracer.println("\{self.func.name} is inlining \{insn}")
            self.replace_call(bb, insn)
            changed = true
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
