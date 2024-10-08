type AnyRegInfo AnyReg

fn AnyRegInfo::output(self : AnyRegInfo, logger : Logger) -> Unit {
  logger.write_string(self._.pretty())
}

typealias UnderlyingAllocator = @lower.PPAllocator[AnyRegInfo]

struct RegAllocator {
  mfn : MFn
  liveness : Liveness
  xctx : RegAllocCtx
  fctx : RegAllocCtx
  color_cache : MutMap[AnyReg, AnyReg]
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

fn RegWrapper::to_reg_id(self : RegWrapper) -> Int {
  self.id
}

fn RegWrapper::is_physical(self : RegWrapper) -> Bool {
  self.r.reg < 0
}

fn RegWrapper::pre_colored(self : RegWrapper) -> Int? {
  self.color
}

fn RegAllocator::new(mfn : MFn, liveness : Liveness) -> RegAllocator {
  let xctx = RegAllocCtx::new(@riscv.preferred_reg_list.length())
  let fctx = RegAllocCtx::new(@riscv.preferred_freg_list.length())
  for i, r in @riscv.preferred_reg_list {
    xctx.allocator.set_color_info(i, r.to_any())
  }
  for i, r in @riscv.preferred_freg_list {
    fctx.allocator.set_color_info(i, r.to_any())
  }
  let color_cache = MutMap::new()
  RegAllocator::{ mfn, liveness, xctx, fctx, color_cache }
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

fn retrive_reg(self : RegAllocCtx, reg : AnyReg) -> RegWrapper? {
  self.reg_memo.get(reg)
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
    self.allocator.set_info(rw, rw.r)
  }
}

fn RegAllocator::assign(self : RegAllocator) -> Unit {
  let reg_ctx_x = self.xctx
  let reg_ctx_f = self.fctx
  for info in self.liveness.iter_block_info {
    let (_, info) = info
    for i, defs in info.defs {
      let live_set = info.live_set[i + 1]
      for defined in defs {
        let reg_ctx = match defined.class {
          X => reg_ctx_x
          F => reg_ctx_f
        }
        if live_set.is_empty() {
          let rw1 = reg_ctx.record_reg(defined)
          reg_ctx.allocator.add_single_vertex(rw1)
        }
        for live in live_set {
          if defined.class == live.class {
            if defined == live {
              let rw1 = reg_ctx.record_reg(defined)
              reg_ctx.allocator.add_single_vertex(rw1)
            } else {
              let rw1 = reg_ctx.record_reg(defined)
              let rw2 = reg_ctx.record_reg(live)
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
  trace(
    self.xctx.allocator.to_dot(
      "integer interference graph of \{self.mfn.name._}",
    ),
  )
  trace(
    self.fctx.allocator.to_dot("float interference graph of \{self.mfn.name._}"),
  )
}

fn RegAllocator::query_assigned_color(
  self : RegAllocator,
  r : AnyReg
) -> AnyReg? {
  if self.color_cache.contains(r) {
    self.color_cache.get(r)
  } else {
    let c = if r.class == X {
      self.xctx
      .retrive_reg(r)
      .bind(
        fn(rw) {
          let color = self.xctx.allocator.query_assigned_color(rw)
          color.map(fn(x) { x._ })
        },
      )
    } else {
      self.fctx
      .retrive_reg(r)
      .bind(
        fn(rw) {
          let color = self.fctx.allocator.query_assigned_color(rw)
          color.map(fn(x) { x._ })
        },
      )
    }
    match c {
      Some(c) => self.color_cache.set(r, c)
      None => ()
    }
    c
  }
}

fn RegAllocator::query_spill(self : RegAllocator, r : AnyReg) -> Int? {
  if r.class == X {
    let rw = self.xctx.retrive_reg(r).unwrap()
    self.xctx.allocator.query_spill(rw)
  } else {
    let rw = self.fctx.retrive_reg(r).unwrap()
    self.fctx.allocator.query_spill(rw)
  }
}

fn RegAllocator::query_all_x_spilled(self : RegAllocator) -> Array[AnyReg] {
  self.xctx.allocator.query_all_spilled().map(fn(x) { x._ })
}

fn RegAllocator::query_all_f_spilled(self : RegAllocator) -> Array[AnyReg] {
  self.fctx.allocator.query_all_spilled().map(fn(x) { x._ })
}

fn RegAllocator::get_clobbered_regs(
  self : RegAllocator
) -> (Set[AnyReg], Set[AnyReg]) {
  let a = self.xctx.allocator.query_all_clobbered()
  let b = self.fctx.allocator.query_all_clobbered()
  (
    Set::from_iter(a.iter().map(fn(x) { x._ })),
    Set::from_iter(b.iter().map(fn(x) { x._ })),
  )
}
