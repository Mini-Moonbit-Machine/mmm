pub type Edge (Int, Int) derive(Show, Eq, Hash)

typealias Set[T] = @hashset.T[T]

typealias Map[K, V] = @hashmap.T[K, V]

pub trait RegToId {
  to_reg_id(Self) -> Int // it is required to be consecutive from 0 to vertex_count - 1
  is_physical(Self) -> Bool // true if it is a physical register
  pre_colored(Self) -> Int? // Some(color) or None
  // why we need is_physical and pre_colored at the same time?
  // it's because some registers like zero, ra, sp are physical but not pre-colored
  // because from the perspective of the allocator, their colors are unusable
}

pub struct PPAllocator[Info] {
  mut vertex_count : Int
  graph : Set[Edge]
  neighbors : Map[Int, Set[Int]]
  phy : Set[Int]
  avail : Int
  info : Map[Int, Info]
  color_info : Map[Int, Info]
  assignment : Map[Int, Int]
  order : Array[Int]
}

pub fn PPAllocator::go[Info : Show](self : PPAllocator[Info]) -> Unit {
  self.mcs()
  self.assign_color()
}

pub fn PPAllocator::query_assigned_color[R : RegToId, Info : Show](
  self : PPAllocator[Info],
  reg : R
) -> Info? {
  self.assignment.get(reg.to_reg_id()).bind(fn(c) { self.color_info.get(c) })
}

pub fn PPAllocator::query_spill[R : RegToId, Info](
  self : PPAllocator[Info],
  reg : R
) -> Int? {
  let r = self.assignment.get(reg.to_reg_id()).unwrap()
  if self.need_spill(r) {
    Some(r - self.avail)
  } else {
    None
  }
}

pub fn PPAllocator::query_all_spilled[Info](self : PPAllocator[Info]) -> Int {
  let spilled = Set::new()
  for asgn in self.assignment {
    let (_, color) = asgn
    if self.need_spill(color) {
      spilled.insert(color - self.avail)
    }
  }
  spilled.size()
}

pub fn PPAllocator::query_all_clobbered[Info](
  self : PPAllocator[Info]
) -> Array[Info] {
  let clobbered = Set::new()
  for asgn in self.assignment {
    let (reg, color) = asgn
    // an assignment for hardware register is just a constraint
    // so it doesn't necessarily mean that the register is clobbered
    if self.need_spill(color) == false && self.phy.contains(reg) == false {
      clobbered.insert(color)
    }
  }
  clobbered.iter().map(fn(c) { self.color_info.get(c).unwrap() }).to_array()
}

pub fn PPAllocator::to_dot[Info : Show](
  self : PPAllocator[Info],
  title : String
) -> String {
  let mut s = "graph G {\n"
  s += "  graph [label=\"\{title}\"];\n"
  let mut asgn = ""
  for info in self.info {
    let reg = info.0
    let info = info.1
    let assigned_color = self.assignment.get(reg)
    if self.phy.contains(reg) {
      s += "  r\{reg} [label=\"\{info}\", color=blue];\n"
    } else {
      match assigned_color {
        Some(c) =>
          if self.need_spill(c) {
            s += "  r\{reg} [label=\"\{info} ! spill\", color=red];\n"
            asgn += "\{info}<-spilled_\{info};"
          } else {
            let color_info = self.color_info.get(c).unwrap()
            s += "  r\{reg} [label=\"\{info} ← \{color_info}\", color=green];\n"
            asgn += "\{info}<-\{color_info};"
          }
        None => s += "  r\{reg} [label=\"\{info}\", color=black];\n"
      }
    }
  }
  for edge in self.graph {
    s += "  r\{edge._.0} -- r\{edge._.1};\n"
  }
  let mut mcs = ""
  for v in self.order {
    let info = self.info.get(v).unwrap()
    if mcs.is_empty() {
      mcs += "\{v}:\{info}"
    } else {
      mcs += "-> \{v}:\{info}"
    }
  }
  s += "  comment = \"\{mcs}\"\n"
  s += "  comment = \"\{asgn}\"\n"
  s += "}\n"
  s
}

pub fn PPAllocator::new[Info](avail : Int) -> PPAllocator[Info] {
  PPAllocator::{
    vertex_count: 0,
    graph: Set::new(),
    neighbors: Map::new(),
    phy: Set::new(),
    avail,
    info: Map::new(),
    color_info: Map::new(),
    assignment: Map::new(),
    order: [],
  }
}

fn PPAllocator::record[R : RegToId, OrigReg](
  self : PPAllocator[OrigReg],
  reg : R
) -> Int {
  let reg_id = reg.to_reg_id()
  if reg_id >= self.vertex_count {
    self.vertex_count = reg_id + 1
  }
  if reg.is_physical() {
    self.phy.insert(reg_id)
    match reg.pre_colored() {
      Some(color) => self.assignment.set(reg_id, color)
      None => ()
    }
  }
  reg_id
}

pub fn PPAllocator::set_info[R : RegToId, Info](
  self : PPAllocator[Info],
  reg : R,
  info : Info
) -> Unit {
  self.info.set(self.record(reg), info)
}

pub fn PPAllocator::set_color_info[Info](
  self : PPAllocator[Info],
  color : Int,
  info : Info
) -> Unit {
  self.color_info.set(color, info)
}

fn PPAllocator::add_neighbor[Info](
  self : PPAllocator[Info],
  a : Int,
  b : Int
) -> Unit {
  fn f(x, y) {
    if x != y {
      match self.neighbors.get(x) {
        Some(neighbors) => neighbors.insert(y)
        None => self.neighbors.set(x, Set::of([y]))
      }
    }
  }

  f(a, b)
  f(b, a)
}

pub fn PPAllocator::add_edge[R : RegToId, Info](
  self : PPAllocator[Info],
  a : R,
  b : R
) -> Unit {
  let a = self.record(a)
  let b = self.record(b)
  self.add_neighbor(a, b)
  let p = if a < b { (a, b) } else { (b, a) }
  self.graph.insert(p)
}

pub fn PPAllocator::add_single_vertex[R : RegToId, Info](
  self : PPAllocator[Info],
  r : R
) -> Unit {
  let r = self.record(r)
  match self.neighbors.get(r) {
    Some(_) => ()
    None => self.neighbors.set(r, Set::new())
  }
}

// Maximum cardinality search

type V (Int, Int) derive(Eq, Show) // (weight, vertex)

impl Compare for V with compare(self, other) {
  Compare::compare(self._.0, other._.0)
}

fn mcs[Info](self : PPAllocator[Info]) -> Unit {
  self.order.clear()
  let wt = []
  let remained = Set::new()
  for v in 0..<self.vertex_count {
    wt.push(V((0, v)))
    remained.insert(v)
  }
  let r = self.order
  fn update_wt(us : Set[Int]) {
    for i, v in wt {
      if us.contains(v._.1) {
        wt[i] = V((v._.0 + 1, v._.1))
      }
    }
  }

  for _ in 0..<self.vertex_count {
    wt.sort()
    let (_, v) = wt.pop().unwrap()._
    remained.remove(v)
    r.push(v)
    let nr = self.neighbors.get(v).unwrap().intersection(remained)
    update_wt(nr)
  }
  if wt.length() != 0 {
    abort("mcs: wt is not empty")
  }
}

fn get_used_color[Info](
  self : PPAllocator[Info],
  neighbors : Set[Int]
) -> Set[Int] {
  let used_colors = Set::new()
  for n in neighbors {
    match self.assignment.get(n) {
      Some(color) => used_colors.insert(color)
      None => ()
    }
  }
  used_colors
}

fn need_spill[Info](self : PPAllocator[Info], color : Int) -> Bool {
  color >= self.avail
}

fn assign_color[Info : Show](self : PPAllocator[Info]) -> Unit {
  let order = self.order
  for r in order {
    if self.phy.contains(r) {
      continue
    }
    let neighbors = self.neighbors.get(r).unwrap()
    let used_colors = self.get_used_color(neighbors)
    let mut candidate = 0
    while true {
      if used_colors.contains(candidate) == false {
        break
      }
      candidate += 1
    }
    self.assignment.set(r, candidate)
  }
}
