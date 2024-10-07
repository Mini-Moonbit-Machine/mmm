typealias UnderlyingAllocator = @lower.PPAllocator[String]

pub struct RegAllocator {
  mfn : MFn
  liveness : Liveness
  xctx : RegAllocCtx
  fctx : RegAllocCtx
}

struct RegWrapper {
  id : Int
  color : Int?
  r : AnyReg
}

fn RegWrapper::new(r : AnyReg, counter : Ref[Int]) -> RegWrapper {
  let id = counter.val
  counter.val += 1
  let color = match r.class {
    X =>
      if r.is_phy_xreg() {
        @riscv.preferred_reg_list.search(r.to_xreg())
      } else {
        None
      }
    F =>
      if r.is_phy_freg() {
        @riscv.preferred_freg_list.search(r.to_freg())
      } else {
        None
      }
  }
  RegWrapper::{ id, r, color }
}

pub fn RegWrapper::to_reg_id(self : RegWrapper) -> Int {
  self.id
}

pub fn RegWrapper::is_physical(self : RegWrapper) -> Bool {
  self.r.reg < 0
}

pub fn RegWrapper::pre_colored(self : RegWrapper) -> Int? {
  self.color
}

fn RegAllocator::new(mfn : MFn, liveness : Liveness) -> RegAllocator {
  let xctx = RegAllocCtx::new(@riscv.preferred_reg_list.length())
  let fctx = RegAllocCtx::new(@riscv.preferred_freg_list.length())
  for i, r in @riscv.preferred_reg_list {
    xctx.allocator.set_color_info(i, r.to_any().pretty())
  }
  for i, r in @riscv.preferred_freg_list {
    fctx.allocator.set_color_info(i, r.to_any().pretty())
  }
  RegAllocator::{ mfn, liveness, xctx, fctx }
}

struct RegAllocCtx {
  counter : Ref[Int]
  reg_memo : MutMap[AnyReg, RegWrapper]
  allocator : UnderlyingAllocator
}

fn record_reg(self : RegAllocCtx, reg : AnyReg) -> RegWrapper {
  match self.reg_memo.get(reg) {
    Some(rw) => rw
    None => {
      let rw = RegWrapper::new(reg, self.counter)
      self.reg_memo.set(reg, rw)
      rw
    }
  }
}

fn RegAllocCtx::new(avail : Int) -> RegAllocCtx {
  let graph = UnderlyingAllocator::new(avail)
  RegAllocCtx::{
    counter: Ref::new(0),
    reg_memo: MutMap::new(),
    allocator: graph,
  }
}

fn RegAllocCtx::set_info(self : RegAllocCtx) -> Unit {
  for rw in self.reg_memo {
    let (_, rw) = rw
    self.allocator.set_info(rw, rw.r.pretty())
  }
}

pub fn RegAllocator::assign(self : RegAllocator) -> Unit {
  let reg_ctx_x = self.xctx
  let reg_ctx_f = self.fctx
  let alloc_reg = @riscv.reg_alloc.to_any() // avoid s11 to be found in the interference graph
  for info in self.liveness.iter_block_info {
    let (_, info) = info
    for live_set in info.live_set {
      for reg1 in live_set {
        for reg2 in live_set {
          if reg1.class == reg2.class && reg1 != alloc_reg && reg2 != alloc_reg {
            if reg1 == reg2 {
              let rw1 = reg_ctx_x.record_reg(reg1)
              reg_ctx_x.allocator.add_single_vertex(rw1)
            } else {
              let reg_ctx = match reg1.class {
                X => reg_ctx_x
                F => reg_ctx_f
              }
              let rw1 = reg_ctx.record_reg(reg1)
              let rw2 = reg_ctx.record_reg(reg2)
              reg_ctx.allocator.add_edge(rw1, rw2)
            }
          }
        }
      }
    }
  }
  reg_ctx_x.set_info()
  reg_ctx_f.set_info()
  self.xctx.allocator.go()
  self.fctx.allocator.go()
  println(
    self.xctx.allocator.to_dot(
      "integer interference graph of \{self.mfn.name._}",
    ),
  )
  println(
    self.fctx.allocator.to_dot("float interference graph of \{self.mfn.name._}"),
  )
}
