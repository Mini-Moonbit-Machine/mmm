type Occurence (Bool, @core.BlockRef) // true means entering, false means exiting

type Ordering Array[Occurence]

pub fn dfs(func : @core.Fn) -> Ordering {
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
  result
}

pub fn Ordering::preorder(self : Ordering) -> Array[@core.BlockRef] {
  self._.filter(fn(x) { x._.0 }).map(fn(x) { x._.1 })
}

pub fn Ordering::postorder(self : Ordering) -> Array[@core.BlockRef] {
  self._.filter(fn(x) { not(x._.0) }).map(fn(x) { x._.1 })
}

pub fn Ordering::rev_preorder(self : Ordering) -> Array[@core.BlockRef] {
  let po = self.preorder()
  po.rev_inplace()
  po
}

pub fn Ordering::rev_postorder(self : Ordering) -> Array[@core.BlockRef] {
  let po = self.postorder()
  po.rev_inplace()
  po
}
