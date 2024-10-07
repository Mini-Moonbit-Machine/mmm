pub struct DominatorTree {
  func : @core.Fn
  dom : @hashmap.T[@core.BlockRef, @core.BlockRef]
  rpo : OrderedBlockRefs
  preds : Predcessors
  mut id_dom : Array[Int]
  mut bref_id_map : @hashmap.T[@core.BlockRef, Int]
  mut id_bref_map : @hashmap.T[Int, @core.BlockRef]
  mut ready : Bool
}

pub fn DominatorTree::new(func : @core.Fn) -> DominatorTree {
  let dom = @hashmap.T::new()
  let bref_id_map = @hashmap.T::new()
  let id_bref_map = @hashmap.T::new()
  let preds = Predcessors::new(func)
  let rpo = Ordering::dfs(func).rev_postorder()
  { func, rpo, preds, dom, bref_id_map, id_bref_map, ready: false, id_dom: [] }
}

pub fn DominatorTree::dominates(
  self : DominatorTree,
  a : @core.BlockRef,
  b : @core.BlockRef
) -> Bool!Failure {
  if self.ready == false {
    self.compute_idom!()
  }
  let a_id = self.bref_id_map.get(a).or_error!(Failure("block not found"))
  let b_id = self.bref_id_map.get(b).or_error!(Failure("block not found"))
  let mut cur_id = b_id
  while cur_id >= a_id {
    if cur_id == a_id {
      return true
    }
    cur_id = self.id_dom[cur_id]
  }
  false
}

pub fn DominatorTree::compute_idom(self : DominatorTree) -> Unit!Failure {
  self.ready = false
  self.dom.clear()
  let rpo = self.rpo
  let preds = self.preds
  let id_dom = Array::make(rpo._.length() + 1, 0)
  let id_bref_map = @hashmap.T::new()
  let bref_id_map = @hashmap.T::new()
  for i, bref in rpo._ {
    id_bref_map[i + 1] = bref
    bref_id_map[bref] = i + 1
  }
  let entry_id = bref_id_map[self.func.entry].or_error!(
    Failure("entry block not found"),
  )
  id_dom[entry_id] = entry_id
  let mut changed = true
  while changed {
    changed = false
    for bref in rpo._ {
      if bref == self.func.entry {
        continue
      }
      let cur_id = bref_id_map.get(bref).or_error!(Failure("bref not found"))
      let pred = preds[bref]
      let mut new_idom = bref_id_map
        .get(pred[0])
        .or_error!(Failure("pred not found"))
      for pred_bref in pred {
        let pred_idom = bref_id_map
          .get(pred_bref)
          .or_error!(Failure("pred not found"))
        if id_dom[pred_idom] != 0 {
          new_idom = intersect(id_dom, new_idom, pred_idom)
        }
      }
      if id_dom[cur_id] != new_idom {
        id_dom[cur_id] = new_idom
        changed = true
      }
    }
  }
  for kv in id_bref_map {
    let (id, bref) = kv
    self.dom[bref] = id_bref_map
      .get(id_dom[id])
      .or_error!(Failure("idom not found"))
  }
  self.ready = true
  self.bref_id_map = bref_id_map
  self.id_bref_map = id_bref_map
  self.id_dom = id_dom
}

fn intersect(id_dom : Array[Int], new_idom : Int, pred_idom : Int) -> Int {
  let mut m = pred_idom
  let mut n = new_idom
  while m != n {
    while m > n {
      m = id_dom[m]
    }
    while n > m {
      n = id_dom[n]
    }
  }
  n
}
