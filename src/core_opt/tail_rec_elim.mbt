pub struct TailRecElim {
  core : @core.Core
  tracer : @util.SubTracer
}

pub fn TailRecElim::new(
  core : @core.Core,
  tracer : @util.SubTracer
) -> TailRecElim {
  { core, tracer }
}

pub struct FnTailRecElim {
  func : @core.Fn
  tracer : @util.SubTracer
}

pub fn FnTailRecElim::new(
  func : @core.Fn,
  tracer : @util.SubTracer
) -> FnTailRecElim {
  { func, tracer }
}

fn FnTailRecElim::find_return(
  self : FnTailRecElim
) -> Array[(@core.BlockRef, Array[@core.Value])] {
  let func = self.func
  let rets = []
  for block in func.blocks {
    let (_, block) = block
    if not(block.is_basic_block()) {
      continue
    }
    let bb = block.bb_or_header_bb(func)
    match bb.control {
      Return(~args) => rets.push((bb.id, args))
      _ => ()
    }
  }
  rets
}

fn FnTailRecElim::check_insn_after_self_call(
  self : FnTailRecElim,
  iref : @core.InsnRef,
  ret_bb : @core.BasicBlock
) -> Bool {
  let start_bb = for block in self.func.blocks {
    let (_, block) = block
    if not(block.is_basic_block()) {
      continue
    }
    let bb = block.bb_or_header_bb(self.func)
    match bb.seq.search(iref) {
      None => ()
      Some(idx) => break Some((bb, idx))
    }
  } else {
    None
  }
  let (start_bb, call_index) = start_bb.unwrap()
  let mut flag = start_bb.seq.length() - 1 == call_index
  let mut cur_bb = start_bb
  while flag {
    match cur_bb.control {
      Jump(~target, ..) =>
        cur_bb = self.func.blocks[target].unwrap().bb_or_header_bb(self.func)
      Return(..) => {
        flag = cur_bb.id == ret_bb.id && cur_bb.seq.is_empty()
        break
      }
      _ => {
        flag = false
        break
      }
    }
  }
  flag
}

fn FnTailRecElim::make_loop(
  self : FnTailRecElim,
  pre_header : @core.BlockRef?
) -> @core.Loop {
  let loop_body = self.func.blocks
    .iter()
    .map(fn(bpair) { bpair.0 })
    .filter(fn(bref) { bref != self.func.entry })
    .collect()

  // there is not any loop before we do tail rec elimination
  let id = @core.BlockRef(self.func.next_block_id())
  let l = @core.Loop::{
    id,
    pre_header,
    header: self.func.entry,
    blocks: loop_body,
  }
  self.func.blocks.set(id, @core.Block::Loop(~l))
  l
}

fn FnTailRecElim::insert_entry_block(
  self : FnTailRecElim,
  loop_bref : @core.BlockRef
) -> Unit {
  let num_params = match self.func.ty {
    DirectFn(~params, ..) => params.length()
    ClosureFn(~params, ..) => params.length()
    _ => self.tracer.abort("not a function type")
  }
  let args = []
  for i in 0..<num_params {
    args.push(@core.fn_param(self.func.id, i))
  }
  let control = @core.Control::Jump(target=loop_bref, ~args)
  let bb = @core.BasicBlock::{
    id: @core.BlockRef(self.func.next_block_id()),
    params: [],
    seq: [],
    control,
  }
  self.func.blocks.set(bb.id, @core.Block::BasicBlock(~bb))
  self.func.entry = bb.id
}

fn FnTailRecElim::find_all_defs_of_value(
  self : FnTailRecElim,
  val : @core.Value
) -> Array[@core.Value] {
  @core_analysis.find_all_defs_of_value(self.func, val)
}

pub fn FnTailRecElim::update_old_entry_basic_block(
  self : FnTailRecElim,
  old_entry : @core.BasicBlock
) -> Unit {
  let params = match self.func.ty {
    DirectFn(~params, ..) => params
    ClosureFn(~params, ..) => params
    _ => self.tracer.abort("not a function type")
  }
  if old_entry.params.length() != 0 {
    self.tracer.abort("old entry block should not have params")
  }
  old_entry.params.clear()
  old_entry.params.append(params)
}

fn FnTailRecElim::replace_tail_rec_call(
  self : FnTailRecElim,
  iref : @core.InsnRef,
  old_entry : @core.BlockRef
) -> Unit {
  let args = match self.func.insns[iref].unwrap() {
    Call(~args, ..) => args
    Apply(~args, ..) => args
    _ => self.tracer.abort("not a call or apply")
  }
  for block in self.func.blocks {
    let (_, block) = block
    if not(block.is_basic_block()) {
      continue
    }
    let bb = block.bb_or_header_bb(self.func)
    match bb.seq.search(iref) {
      Some(idx) => {
        let _ = bb.seq.remove(idx)
        bb.control = Jump(target=old_entry, ~args)
      }
      None => ()
    }
  }
}

fn FnTailRecElim::replace_parameters(
  self : FnTailRecElim,
  new_entry : @core.BlockRef,
  old_entry : @core.BlockRef
) -> Unit {
  let num_params = match self.func.ty {
    DirectFn(~params, ..) => params.length()
    ClosureFn(~params, ..) => params.length()
    _ => self.tracer.abort("not a function type")
  }
  let params = []
  for i in 0..<num_params {
    params.push(
      (@core.fn_param(self.func.id, i), @core.block_param(old_entry, i)),
    )
  }
  for block in self.func.blocks {
    let (_, block) = block
    if not(block.is_basic_block()) {
      continue
    }
    let bb = block.bb_or_header_bb(self.func)
    if bb.id != new_entry {
      params.each(
        fn(p) {
          let (fp, bp) = p
          fp.replace_all_uses_with(self.func, bb, bp)
        },
      )
    }
  }
}

fn FnTailRecElim::elim_tail_rec(self : FnTailRecElim) -> Unit {
  let rets = self.find_return()
  let tail_rec_calls = []
  for ret in rets {
    let (ret_bb, args) = ret
    let ret_bb = self.func.blocks[ret_bb].unwrap().bb_or_header_bb(self.func)
    if args.length() == 1 {
      let defs = self.find_all_defs_of_value(args[0])
      for def in defs {
        match def {
          Insn(~iref) => {
            let insn = self.func.insns[iref].unwrap()
            match insn {
              Call(~f, ..) =>
                if f == self.func.id &&
                  self.check_insn_after_self_call(iref, ret_bb) {
                  tail_rec_calls.push(iref)
                }
              Apply(f=Self(..), ..) =>
                if self.check_insn_after_self_call(iref, ret_bb) {
                  tail_rec_calls.push(iref)
                }
              _ => ()
            }
          }
          _ => ()
        }
      }
    }
  }
  if tail_rec_calls.is_empty() {
    return
  }
  let old_entry = self.func.entry
  let l = self.make_loop(None)
  self.insert_entry_block(l.id)
  l.pre_header = Some(self.func.entry)
  self.update_old_entry_basic_block(
    self.func.blocks[old_entry].unwrap().bb_or_header_bb(self.func),
  )
  for iref in tail_rec_calls {
    self.replace_tail_rec_call(iref, old_entry)
    self.tracer.println(
      "eliminated tail rec call \{iref}: \{self.func.insns[iref].unwrap()}",
    )
    self.func.insns.remove(iref)
  }
  self.replace_parameters(self.func.entry, old_entry)
}

pub fn go(self : TailRecElim) -> Unit {
  for func in self.core.funcs {
    let (_, func) = func
    let ftre = FnTailRecElim::new(func, self.tracer)
    ftre.elim_tail_rec()
  }
}
