pub type Predcessors (
  @core.FnRef,
  @hashmap.T[@core.BlockRef, Array[@core.BlockRef]],
) derive(Show)

pub fn Predcessors::op_get(
  self : Predcessors,
  key : @core.BlockRef
) -> Array[@core.BlockRef] {
  self._.1.get_or_default(key, [])
}

pub fn Predcessors::new(func : @core.Fn) -> Predcessors {
  let map = @hashmap.T::new()
  for bpair in func.blocks {
    let (_, block) = bpair
    match block {
      @core.Block::BasicBlock(~bb) => {
        let succs = bb.control.successors()
        for succ in succs {
          match map.get(succ) {
            None => map.set(succ, [bb.id])
            Some(preds) => preds.push(bb.id)
          }
        }
      }
      _ => () // ignore loop because its inner blocks are already in the map
    }
  }
  (func.id, map)
}
