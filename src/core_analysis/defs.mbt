pub fn find_all_defs_of_value(
  func : @core.Fn,
  val : @core.Value
) -> Array[@core.Value] {
  let res = []
  let worklist = []
  let visited = @hashset.new()
  fn push_or_pend(val : @core.Value) {
    match val {
      BlockParam(~bref, ~bpref) => {
        if visited.contains(val) {
          return
        }
        worklist.push((bref, bpref))
        visited.insert(val)
      }
      _ => res.push(val)
    }
  }

  fn handle_jump(
    target : @core.BlockRef,
    bref : @core.BlockRef,
    bpref : @core.BlockParamRef,
    args : Array[@core.Value]
  ) {
    let target = func.blocks[target].unwrap().bb_or_header_bb(func).id
    if target == bref {
      let val = args[bpref.id]
      push_or_pend(val)
    }
  }

  fn find_indirect_defs() {
    let (bref, bpref) = worklist.pop().unwrap()
    for block in func.blocks {
      let (_, block) = block
      if not(block.is_basic_block()) {
        continue
      }
      let bb = block.bb_or_header_bb(func)
      match bb.control {
        Jump(~target, ~args) => handle_jump(target, bref, bpref, args)
        Branch(~t, ~f, ~t_args, ~f_args, ..) => {
          handle_jump(t, bref, bpref, t_args)
          handle_jump(f, bref, bpref, f_args)
        }
        Select(~targets, ~args, ..) =>
          for target in targets {
            handle_jump(target, bref, bpref, args)
          }
        Panic(..) => ()
        Return(..) => ()
      }
    }
  }

  push_or_pend(val)
  while worklist.is_empty() == false {
    find_indirect_defs()
  }
  res
}
