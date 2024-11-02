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

test "order" {
  //            0
  //          /   \
  //         1     2
  //        /  \     \
  //       3    4    |
  //       |\  _|____|
  //       | \/ |
  //       | /\ |
  //       5    6
  //
  // (3 -> 5, and 3 -> 6 are critical edges and must be split)
  //
  let bb0 = bb(0, branch(1, 2))
  let bb1 = bb(1, branch(3, 4))
  let bb2 = bb(2, jump(5))
  let bb3 = bb(3, branch(5, 6))
  let bb4 = bb(4, jump(6))
  let bb5 = bb(5, panic())
  let bb6 = bb(6, panic())
  let func = func([bb0, bb1, bb2, bb3, bb4, bb5, bb6])
  let order = @lower.LoweringOrder::new(func, @util.t)
  println(order)
}
