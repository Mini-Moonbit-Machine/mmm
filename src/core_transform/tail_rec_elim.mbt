struct TailRecEliminator {
  core_env : @core.CoreEnv
  mut header : @core.BlockRef?
}

let trace_flag = false

fn trace(msg : String) -> Unit {
  if trace_flag {
    println(msg)
  }
}

fn TailRecEliminator::find_candidate_call(
  func : @core.Fn,
  block : @core.BasicBlock
) -> (Int, @core.InsnRef)? {
  // backward from terminator, checking whether there is tail call
  for rev_index, insn in block.seq.rev().iter2() {
    guard let Some(instruction) = func.insns.get(insn) else { _ => continue }
    guard let @core.Call(f=fid, ..)
    | @core.Apply(f=@core.Value::Fn(fref=fid), ..)
    | @core.Apply(f=@core.Value::Self(fref=fid), ..) = instruction else {
      _ => continue
    }
    if fid == func.id {
      break Some((block.seq.length() - rev_index - 1, insn))
    }
  } else {
    None
  }
}

fn TailRecEliminator::create_loop_header(
  self : TailRecEliminator,
  func : @core.Fn
) -> @core.BlockRef!Failure {
  let func_entry = @core.Block::basic_block(
    func.blocks.get(func.entry).unwrap(),
    func,
  )
  // Let header parameters simulate function parameters.
  let func_param_types = match func.ty {
    @core.ClosureFn(~params, ..) | @core.DirectFn(~params, ..) => params
    _ => fail!("not a function (closure) type")
  }
  // Update loop header block parameters.
  func_entry.params.clear()
  func_entry.params.append(func_param_types)
  let func_args = []
  for i = 0, len = func_param_types.length(); i < len; i = i + 1 {
    let param = @core.FnParam(
      fref=func.id,
      fpref=@core.FnParamRef(@core.Id::new(i)),
    )
    func_args.push(param)
  }

  // Update the use of function argument.
  for arg_index, func_arg in func_args.iter2() {
    let new_arg = @core.BlockParam(
      bref=func_entry.id,
      bpref=@core.BlockParamRef(@core.Id::new(arg_index)),
    )
    func_arg.replace_all_uses_with(func, new_arg)
  }

  // TODO: Handle return value?
  let new_entry = self.core_env.add_basic_block(func, func_param_types)
  trace("Create Loop Header .bb #\{new_entry.id} for .fn #\{func.name}")
  new_entry.control = @core.Jump(target=func.entry, args=func_args)
  func.entry = new_entry.id
  func_entry.id
}

fn can_move_above_call(insn : @core.Insn, callref : @core.InsnRef) -> Bool {
  let insn_operands = insn.operands()
  match insn {
    @core.Store(..) => false // Alias analysis required for more precision.
    _ =>
      // Otherwise, if this is a side-effect free instruction, check to make sure
      // that it does not use the return value of the call.  If it doesn't use the
      // return value of the call, it must only use things that are defined before
      // the call, or movable instructions between the call and the instruction
      // itself.
      for op in insn_operands {
        guard let @core.Value::Insn(iref=insnid) = op else { _ => continue }
        if insnid == callref {
          break false
        }
      } else {
        true
      }
  }
}

fn TailRecEliminator::eliminate_call(
  self : TailRecEliminator,
  func : @core.Fn,
  block : @core.BasicBlock,
  call_site : (Int, @core.InsnRef)
) -> Unit!Failure {
  // Check if all of the instructions between the call and the return are
  // movable to above the call itself, leaving the call next to the return.
  // TODO: recognize the accumulator pattern of commutative operators.
  let call_index = call_site.0
  let call_insn = call_site.1
  for insnref in block.seq[call_index + 1:] {
    let insn = func.insns.get(insnref).unwrap()
    if not(can_move_above_call(insn, call_insn)) {
      return
    }
  }

  // Require the new entry block if it does not exist,
  // allowing us to branch back to the old entry.
  let loop_header = match self.header {
    Some(header) => header
    None => {
      let header = self.create_loop_header!(func)
      self.header = Some(header)
      header
    }
  }

  // remove the call and return instructions, replacing them with jump
  let call = func.insns.get(call_insn).unwrap()
  let rec_args = match call {
    @core.Call(~args, ..) => args
    @core.Apply(~args, ..) => args
    _ => fail!("unexpected call instruction")
  }
  let unconditional_jump = @core.Jump(target=loop_header, args=rec_args)
  block.control = unconditional_jump
  block.remove_insn(call_index, func)
  trace(
    "Eliminate Tail Call at instruction \{call_index}, .bb #\{block.id} of .fn #\{func.name}",
  )
}

fn TailRecEliminator::process_block(
  self : TailRecEliminator,
  func : @core.Fn,
  block : @core.BasicBlock
) -> Unit!Failure {
  trace("Process Block .bb #\{block.id} of .fn #\{func.name}")
  let terminator = block.control
  match terminator {
    @core.Jump(~target, args=jump_args) => {
      // TODO: simplify CFG first
      // workaround for brainfuck interpreter test input, filter empty blocks
      let potential_return_bb = for blockref = target {
        let bb = @core.Block::basic_block(
          func.blocks.get(blockref).unwrap(),
          func,
        )
        // empty block with unconditional jump
        match (bb.seq.length(), bb.control) {
          (0, @core.Jump(target=succ, ..)) => continue succ
          _ => break blockref
        }
      }
      // Case I: BB1: call self(); ...; jump BB2. BB2: return v;
      // check the succcessor block as the candidate tail call
      let succ = @core.Block::basic_block(
        func.blocks.get(potential_return_bb).unwrap(),
        func,
      )
      println("potential_return_bb of \{block.id}:  \{succ.id}")
      // if the only valid instruction is return
      if succ.seq.length() > 0 {
        return
      }
      guard let @core.Return(args=ret_args) = succ.control else { _ => return  }
      let candidate_call_site = find_candidate_call(func, block)
      guard let Some(call_site) = candidate_call_site else { _ => return  }
      trace(
        "Find Tail Call at instruction \{call_site.0}, .bb #\{block.id} of .fn #\{func.name}",
      )

      // fold the single ret in the successor block into the current block,
      // and adjust the ret argument (block parameters) and cfg.
      // canonicalize to the form that call and return in the same basic block:
      // BB1: call self(); ...; return v';
      let new_ret_args = ret_args.copy()
      for index, ret_arg in new_ret_args.iter2() {
        guard let @core.BlockParam(bref=bid, ..) = ret_arg else {
          _ => continue
        }
        if bid == succ.id {
          // replace by jump argument
          new_ret_args[index] = jump_args[index]
        }
      }
      let new_ret = @core.Return(args=new_ret_args)
      block.control = new_ret

      // eliminate the tail call
      self.eliminate_call!(func, block, call_site)
    }
    @core.Return(..) => {
      // Case II: BB1: call self(); ...; return v;
      let candidate_call_site = find_candidate_call(func, block)
      guard let Some(call_site) = candidate_call_site else { _ => return  }
      trace(
        "Find Tail Call at instruction \{call_site.0}, .bb #\{block.id} of .fn #\{func.name}",
      )
      self.eliminate_call!(func, block, call_site)
    }
    _ => ()
  }
}

pub fn tail_recursive_elimination(
  func : @core.Fn,
  ctx : @core.CoreEnv // richer context management in the future
) -> Unit!Failure {
  let eliminator = TailRecEliminator::{ core_env: ctx, header: None }
  for bpair in func.blocks {
    let (_, block) = bpair
    // TODO: collect lists of updates
    guard let @core.Block::BasicBlock(~bb) = block else { _ => continue }
    ctx.bind_function(func)
    eliminator.process_block!(func, bb)
    bb.pack_indices()
  }
}

pub struct TailRecElimPass {
  ctx : @core.CoreEnv
}

pub fn TailRecElimPass::with_ctx(ctx : @core.CoreEnv) -> TailRecElimPass {
  TailRecElimPass::{ ctx, }
}

pub fn TailRecElimPass::run(
  self : TailRecElimPass,
  func : @core.Fn
) -> Unit!Failure {
  tail_recursive_elimination!(func, self.ctx)
}
