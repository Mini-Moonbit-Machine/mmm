struct InsnInsert {
  index : Int
  insn : @core.InsnRef
}

pub typealias InsertSet = Array[InsnInsert]

pub fn sort_by_index(insertions : InsertSet) -> Unit {
  insertions.sort_by_key(fn(insert) { insert.index })
}

pub fn bulk_insertion(
  block : @core.BasicBlock,
  insertion_set : InsertSet
) -> Unit {
  let num_insertion = insertion_set.length()
  // fast path if there is no insertion
  if num_insertion == 0 {
    return
  }
  block.seq.reserve_capacity(block.seq.length() + num_insertion)
  for last_index = block.seq.length(), insert_index = num_insertion
      insert_index > 0
      insert_index = insert_index - 1 {
    let first_index = insertion_set[insert_index].index + insert_index
    let index_offset = insert_index - 1
    for i = last_index - 1; i >= first_index; i = i - 1 {
      block.seq[i] = block.seq[i - index_offset]
    }
    block.seq[first_index] = insertion_set[insert_index].insn
    continue first_index, insert_index - 1
  }
}

pub trait LocalPass {
  run(Self, @core.Fn) -> Unit!Failure
  name(Self) -> String
}

pub trait GlobalPass {
  run(Self, @core.Core) -> Unit!Failure
  name(Self) -> String
}

pub enum Pass {
  Local(LocalPass)
  Global(GlobalPass)
}

struct PassPipeline {
  passes : Array[Pass]
}

pub fn PassPipeline::new() -> PassPipeline {
  PassPipeline::{ passes: Array::new() }
}

pub fn PassPipeline::with_passes(passes : Array[Pass]) -> PassPipeline {
  PassPipeline::{ passes, }
}

pub fn PassPipeline::add_pass(self : PassPipeline, pass : Pass) -> Unit {
  self.passes.push(pass)
}

pub fn PassPipeline::run(
  self : PassPipeline,
  core : @core.Core,
  ~is_debug : Bool = false
) -> Unit!Failure {
  for pass in self.passes {
    match pass {
      Pass::Local(local_pass) =>
        for fpair in core.funcs {
          let (_, func) = fpair
          if is_debug {
            println("[Core Pass]: \{local_pass.name()} on \{func.name}")
          }
          local_pass.run!(func)
        }
      Pass::Global(global_pass) => {
        if is_debug {
          println("[Core Pass]: \{global_pass.name()}")
        }
        global_pass.run!(core)
      }
    }
    if is_debug {
      println("\{core.to_pretty_print()}")
    }
  }
}
