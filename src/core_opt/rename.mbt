pub struct FnCounter {
  fvar_counter : Int
  insn_counter : Int
  block_counter : Int
  mem_counter : Int
}

pub fn FnCounter::new() -> FnCounter {
  { fvar_counter: 0, insn_counter: 0, block_counter: 0, mem_counter: 0 }
}

pub fn FnCounter::new_from_existing(f : @core.Fn) -> FnCounter {
  let mut id = -1
  let fvar_counter = f.fvars.length()
  for insn in f.insns {
    let (id_used, _) = insn
    id = @math.maximum(id, id_used.id)
  }
  let insn_counter = id + 1
  for block in f.blocks {
    let (id_used, _) = block
    id = @math.maximum(id, id_used.id)
  }
  let block_counter = id + 1
  for mem in f.mems {
    let (id_used, _) = mem
    id = @math.maximum(id, id_used.id)
  }
  let mem_counter = id + 1
  { fvar_counter, insn_counter, block_counter, mem_counter }
}

pub struct Renamer {
  func : @core.Fn
  counter : FnCounter
  tracer : @util.SubTracer
}

pub fn Renamer::new(
  func : @core.Fn,
  counter : FnCounter,
  tracer : @util.SubTracer
) -> Renamer {
  { func, counter, tracer }
}

fn shift_value(self : Renamer, value : @core.Value) -> @core.Value {
  match value {
    Unit => value
    Bool(..) => value
    Int32(..) => value
    Int64(..) => value
    Double(..) => value
    Insn(~iref) => Insn(iref=iref._.shift(self.counter.insn_counter))
    Mem(~fref, ~mref) => Mem(~fref, mref=mref._.shift(self.counter.mem_counter))
    Fn(..) => value
    FreeVar(..) => value
    BlockParam(~bref, ~bpref) =>
      BlockParam(bref=bref._.shift(self.counter.block_counter), ~bpref)
    FnParam(~fref, ~fpref) => FnParam(~fref, ~fpref)
    Self(..) => value
  }
}

fn shift_mem(self : Renamer, mem : @core.Mem) -> @core.Mem {
  match mem {
    Stack(~ty, ~len) => Stack(~ty, len=self.shift_value(len))
    Heap(~ty, ~len) => Heap(~ty, len=self.shift_value(len))
  }
}

fn shift_control(self : Renamer, control : @core.Control) -> @core.Control {
  match control {
    Jump(~target, ~args) =>
      Jump(
        target=target._.shift(self.counter.block_counter),
        args=args.map(fn(arg) { self.shift_value(arg) }),
      )
    Return(~args) => Return(args=args.map(fn(arg) { self.shift_value(arg) }))
    Branch(~cond, ~t, ~t_args, ~f, ~f_args) =>
      Branch(
        cond=self.shift_value(cond),
        t=t._.shift(self.counter.block_counter),
        t_args=t_args.map(fn(arg) { self.shift_value(arg) }),
        f=f._.shift(self.counter.block_counter),
        f_args=f_args.map(fn(arg) { self.shift_value(arg) }),
      )
    Select(~cond, ~targets, ~fallback) =>
      Select(
        cond=self.shift_value(cond),
        targets=targets.map(
          fn(target) { target._.shift(self.counter.block_counter) },
        ),
        fallback=fallback._.shift(self.counter.block_counter),
      )
    Panic => Panic
  }
}

fn shift_insn(self : Renamer, insn : @core.Insn) -> @core.Insn {
  let insn = insn.replace(fn(value) { self.shift_value(value) })
  match insn {
    Alloc(~mref, ~ty, ~fills) =>
      Alloc(mref=mref._.shift(self.counter.mem_counter), ~ty, ~fills)
    _ => insn
  }
}

fn shift_basic_block(self : Renamer, bb : @core.BasicBlock) -> @core.BasicBlock {
  {
    id: bb.id._.shift(self.counter.block_counter),
    params: bb.params,
    seq: bb.seq.map(fn(iref) { iref._.shift(self.counter.insn_counter) }),
    control: self.shift_control(bb.control),
  }
}

fn shift_loop(self : Renamer, lp : @core.Loop) -> @core.Loop {
  {
    id: lp.id._.shift(self.counter.block_counter),
    pre_header: lp.pre_header.map(
      fn(p) { p._.shift(self.counter.block_counter) },
    ),
    header: lp.header._.shift(self.counter.block_counter),
    blocks: lp.blocks.map(fn(b) { b._.shift(self.counter.block_counter) }),
  }
}

fn shift_block(self : Renamer, block : @core.Block) -> @core.Block {
  match block {
    BasicBlock(~bb) => BasicBlock(bb=self.shift_basic_block(bb))
    Loop(~l) => Loop(l=self.shift_loop(l))
  }
}

pub fn Renamer::shift(self : Renamer) -> @core.Fn {
  let id = self.func.id
  let insns : Iter[(@core.InsnRef, @core.Insn)] = self.func.insns
    .iter()
    .map(
      fn(insn) {
        let (id, insn) = insn
        let id = id._.id + self.counter.insn_counter
        (@core.Id::new(id), self.shift_insn(insn))
      },
    )
  let insns = @hashmap.T::from_iter(insns)
  let mems : Iter[(@core.MemRef, @core.Mem)] = self.func.mems
    .iter()
    .map(
      fn(mem) {
        let (id, mem) = mem
        let id = id._.id + self.counter.mem_counter
        (@core.Id::new(id), self.shift_mem(mem))
      },
    )
  let mems = @hashmap.T::from_iter(mems)
  let blocks : Iter[(@core.BlockRef, @core.Block)] = self.func.blocks
    .iter()
    .map(
      fn(block) {
        let (id, block) = block
        let id = id._.id + self.counter.block_counter
        (@core.Id::new(id), self.shift_block(block))
      },
    )
  let blocks = @hashmap.T::from_iter(blocks)
  {
    id,
    ty: self.func.ty,
    name: self.func.name,
    fvars: self.func.fvars,
    insns,
    blocks,
    mems,
    entry: @core.BlockRef(
      @core.Id::new(self.func.entry._.id + self.counter.block_counter),
    ),
  }
}
