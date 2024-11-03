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

pub fn target_bb(func : @core.Fn, target : @core.BlockRef) -> @core.BlockRef {
  let target = func.blocks.get(target).unwrap()
  let target = target.bb_or_header_bb(func).id
  target
}

pub fn FnDropLoopParams::go(self : FnDropLoopParams) -> Unit {
  let mut changed = true
  let loops = []
  for block in self.func.blocks {
    let (_, block) = block
    guard let @core.Block::Loop(..) = block else { _ => continue }
    loops.push(block)
  }
  while changed {
    changed = false
    for block in loops {
      guard let @core.Block::Loop(~l) = block else {
        _ => self.tracer.abort("unreachable")
      }
      let header = l.header
      let header_bb = self.func.blocks
        .get(header)
        .unwrap()
        .bb_or_header_bb(self.func)
      let loop_blocks = block.all_basic_blocks(self.func)
      self.tracer.println("Loop blocks of \{l.id}: \{loop_blocks}")
      let loop_insns = loop_blocks
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
          Insn(~iref) => false
          _ => false
        }
      }

      for i in 0..<header_bb.params.length() {
        let bp = @core.block_param(header, i)
        let defs = @core_analysis.find_all_defs_of_value(self.func, bp)
        let defs = @hashset.from_iter(defs.iter())
        self.tracer.println("Defs of block param \{bp}: \{defs}")
        if defs.iter().all(fn(val) { can_be_dropped(val, header, i) }) {
          drop_list.push(bp)
        }
      }
      self.tracer.println("Dropping loop params: \{drop_list}")
      let drop_set = @hashset.from_iter(
        drop_list
        .iter()
        .map(
          fn(bp) {
            guard let BlockParam(~bpref, ..) = bp else {
              _ => self.tracer.abort("unreachable")
            }
            bpref._.id
          },
        ),
      )
      if drop_set.is_empty() {
        continue
      } else {
        changed = true
        // we need to drop these block parameters in the header block
        // REPLACE them with defs in the PRE-HEADER block
        let new_params = []
        for i, old_pty in header_bb.params {
          if drop_set.contains(i) {
            continue
          }
          new_params.push(old_pty)
        }
        self.tracer.println(
          "Header params: \{header_bb.params} -> \{new_params}",
        )
        header_bb.params = new_params
        let pre_header = l.pre_header.or_else(
          fn() { self.tracer.abort("Loop should have a pre-header") },
        )
        let pre_header_bb = self.func.blocks
          .get(pre_header)
          .unwrap()
          .bb_or_header_bb(self.func)
        fn drop_control_args(args : Array[@core.Value]) {
          let new_args = []
          for i, old_arg in args {
            if drop_set.contains(i) {
              continue
            }
            new_args.push(old_arg)
          }
          self.tracer.println(
            "Dropping control args: \{new_args.length() - args.length()} \{args} -> \{new_args}",
          )
          new_args
        }

        for block in self.func.blocks {
          let (_, block) = block
          guard let @core.Block::BasicBlock(~bb) = block else { _ => continue }
          if bb.id == pre_header {
            continue
          }
          match bb.control {
            @core.Control::Jump(~args, ~target) =>
              if target_bb(self.func, target) == header {
                bb.control = Jump(args=drop_control_args(args), ~target)
              }
            Branch(~cond, ~t, ~t_args, ~f, ~f_args) => {
              let mut t_args = t_args
              let mut f_args = f_args
              if target_bb(self.func, t) == header {
                t_args = drop_control_args(t_args)
              }
              if target_bb(self.func, f) == header {
                f_args = drop_control_args(f_args)
              }
              bb.control = Branch(~cond, ~t, ~t_args, ~f, ~f_args)
            }
            Return(..) | Panic => ()
            _ => self.tracer.abort("not implemented")
          }
        }
        guard let Jump(~args, ~target) = pre_header_bb.control else {
          _ => self.tracer.abort("pre-header should have a jump")
        }
        let new_args = []
        let subst = {}
        for i, old_arg in args {
          if drop_set.contains(i) {
            subst[@core.block_param(header, i)] = old_arg
            continue
          }
          new_args.push(old_arg)
        }
        self.tracer.println(
          "Dropping pre-header args: \{new_args.length() - args.length()} \{args} -> \{new_args}",
        )
        pre_header_bb.control = Jump(args=new_args, ~target)
        self.tracer.println("Subst dropped params: \{subst}")

        // replace all uses of the dropped block params
        for block in self.func.blocks {
          let (_, block) = block
          guard let @core.Block::BasicBlock(~bb) = block else { _ => continue }
          @core.replace_all(
            self.func,
            bb,
            map=fn(val) { subst.get(val).or(val) },
          )
        }
        self.tracer.println("After dropping loop params:")
        self.tracer.println(self.func.to_pretty_print())
        break
      }
    }
  }
}
