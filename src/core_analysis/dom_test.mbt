fn id(id : Int) -> @core.Id {
  @core.Id::{ id, }
}

fn jump(target : Int) -> @core.Control {
  @core.Control::Jump(target=id(target), args=[])
}

fn branch(t : Int, f : Int) -> @core.Control {
  @core.Control::Branch(
    cond=@core.Value::Bool(val=true),
    t=id(t),
    t_args=[],
    f=id(f),
    f_args=[],
  )
}

fn panic() -> @core.Control {
  @core.Control::Panic
}

fn bb(bid : Int, control : @core.Control) -> @core.BasicBlock {
  @core.BasicBlock::{ id: id(bid), params: [], control, seq: [] }
}

fn func(blocks : Array[@core.BasicBlock]) -> @core.Fn {
  let blocks = @hashmap.T::from_array(
    blocks.map(fn(bb) { (bb.id, @core.Block::BasicBlock(~bb)) }),
  )
  @core.Fn::{
    id: @core.FnRef(id(0)),
    entry: @core.BlockRef(id(0)),
    name: "test",
    ty: @core.Type::Unit,
    fvars: [],
    blocks,
    insns: @hashmap.T::new(),
    mems: @hashmap.T::new(),
  }
}

test "dominator" {
  let bb0 = bb(0, jump(2))
  let bb1 = bb(1, panic())
  let bb2 = bb(2, jump(1))
  let func = func([bb0, bb1, bb2])
  let dom = @core_analysis.DominatorTree::new(func, @util.t)
  dom.compute_idom!()
  assert_true!(dom.dom.get(id(0)) == Some(id(0)))
  assert_true!(dom.dom.get(id(1)) == Some(id(2)))
  assert_true!(dom.dom.get(id(2)) == Some(id(0)))
  assert_true!(dom.dominates!(id(0), id(0)))
  assert_true!(dom.dominates!(id(0), id(1)))
  assert_true!(dom.dominates!(id(0), id(2)))
  assert_true!(dom.dominates!(id(1), id(1)))
  assert_true!(dom.dominates!(id(2), id(2)))
  assert_true!(dom.dominates!(id(2), id(1)))
}

test "dominator2" {
  let bb0 = bb(0, branch(1, 2))
  let bb1 = bb(1, jump(3))
  let bb2 = bb(2, jump(3))
  let bb3 = bb(3, panic())
  let func = func([bb0, bb1, bb2, bb3])
  let dom = @core_analysis.DominatorTree::new(func, @util.t)
  dom.compute_idom!()
  assert_true!(dom.dom.get(id(0)) == Some(id(0)))
  assert_true!(dom.dom.get(id(1)) == Some(id(0)))
  assert_true!(dom.dom.get(id(2)) == Some(id(0)))
  assert_true!(dom.dom.get(id(3)) == Some(id(0)))
  assert_true!(dom.dominates!(id(0), id(0)))
  assert_true!(dom.dominates!(id(0), id(1)))
  assert_true!(dom.dominates!(id(0), id(2)))
  assert_true!(dom.dominates!(id(0), id(3)))
  assert_true!(dom.dominates!(id(1), id(1)))
  assert_false!(dom.dominates!(id(1), id(3)))
  assert_true!(dom.dominates!(id(2), id(2)))
  assert_false!(dom.dominates!(id(2), id(3)))
}

test "dominator 3" {
  let bb0 = bb(0, jump(1))
  let bb1 = bb(1, branch(2, 5))
  let bb2 = bb(2, jump(3))
  let bb3 = bb(3, branch(4, 1))
  let bb4 = bb(4, panic())
  let bb5 = bb(5, branch(6, 8))
  let bb6 = bb(6, jump(7))
  let bb7 = bb(7, jump(3))
  let bb8 = bb(8, jump(7))
  let func = func([bb0, bb1, bb2, bb3, bb4, bb5, bb6, bb7, bb8])
  let dom = @core_analysis.DominatorTree::new(func, @util.t)
  dom.compute_idom!()
  assert_true!(dom.dom.get(id(0)) == Some(id(0)))
  assert_true!(dom.dom.get(id(1)) == Some(id(0)))
  assert_true!(dom.dom.get(id(2)) == Some(id(1)))
  assert_true!(dom.dom.get(id(3)) == Some(id(1)))
  assert_true!(dom.dom.get(id(4)) == Some(id(3)))
  assert_true!(dom.dom.get(id(5)) == Some(id(1)))
  assert_true!(dom.dom.get(id(6)) == Some(id(5)))
  assert_true!(dom.dom.get(id(7)) == Some(id(5)))
  assert_true!(dom.dom.get(id(8)) == Some(id(5)))
}
