pub type Occurence (Bool, @core.BlockRef) // true means entering, false means exiting

pub type Ordering (@core.Fn, Array[Occurence])

pub type OrderedBlockRefs (@core.Fn, Array[@core.BlockRef])

pub type OrderedBasicBlocks Array[@core.BasicBlock]

pub fn Ordering::dfs(func : @core.Fn) -> Ordering {
  let visited = @hashset.T::new()
  let stack = [(true, func.entry)]
  let result = []
  fn dfs_block(block : @core.Block) {
    match block {
      @core.Block::BasicBlock(~bb) => {
        let succs = bb.control.successors()
        succs.rev_inplace()
        for succ in succs {
          if visited.contains(succ) == false {
            stack.push((true, succ))
          }
        }
      }
      @core.Block::Loop(~l) => {
        let hbref = l.header
        let hblock = func.blocks[hbref].unwrap()
        dfs_block(hblock)
      }
    }
  }

  while not(stack.is_empty()) {
    let (is_enter, bref) as item = match stack.pop() {
      Some(x) => x
      None => break
    }
    if is_enter && visited.contains(bref) == false {
      visited.insert(bref)
      stack.push((false, bref))
      let block = func.blocks[bref].unwrap()
      dfs_block(block)
    }
    result.push(Occurence(item))
  }
  (func, result)
}

pub fn Ordering::preorder(self : Ordering) -> OrderedBlockRefs {
  (self._.0, self._.1.filter(fn(x) { x._.0 }).map(fn(x) { x._.1 }))
}

pub fn Ordering::postorder(self : Ordering) -> OrderedBlockRefs {
  (self._.0, self._.1.filter(fn(x) { not(x._.0) }).map(fn(x) { x._.1 }))
}

pub fn Ordering::rev_preorder(self : Ordering) -> OrderedBlockRefs {
  let po = self.preorder()
  po._.1.rev_inplace()
  po
}

pub fn Ordering::rev_postorder(self : Ordering) -> OrderedBlockRefs {
  let po = self.postorder()
  po._.1.rev_inplace()
  po
}

pub fn OrderedBlockRefs::to_basic_blocks(
  self : OrderedBlockRefs
) -> OrderedBasicBlocks {
  let f = self._.0
  self._.1.map(
    fn(bref) {
      match f.blocks[bref] {
        Some(@core.Block::BasicBlock(~bb)) => bb
        _ => abort("not a basic block")
      }
    },
  )
}
