pub type Edge (Int, Int) derive(Show, Eq, Hash)

typealias Set[T] = @hashset.T[T]

typealias PriotyQueue[T] = @priority_queue.T[T]

typealias Map[K, V] = @hashmap.T[K, V]

pub trait RegToId {
  to_reg_id(Self) -> Int // it is required to be consecutive from 0 to vertex_count - 1
  is_physical(Self) -> Bool // true if it is a physical register
  pre_colored(Self) -> Int? // Some(color) or None
  // why we need is_physical and pre_colored at the same time?
  // it's because some registers like zero, ra, sp are physical but not pre-colored
  // because from the perspective of the allocator, their colors are unusable
}

type ColorFreq (Int, Int) derive(Eq)

impl Compare for ColorFreq with compare(self, other) {
  // first compare by frequency, we prefer the one with lower frequency
  let cmp = -self._.1.compare(other._.1)
  if cmp == 0 {
    // then compare by color, we prefer the one with higher color
    self._.0.compare(other._.0)
  } else {
    cmp
  }
}

pub struct PPAllocator[Info] {
  mut vertex_count : Int
  graph : Set[Edge] // vertex -> vertex
  neighbors : Map[Int, Set[Int]] // vertex -> [vertex]
  phy : Set[Int] // physical registers
  info : Map[Int, Info] // reg -> info
  color_info : Map[Int, Info] // color -> info
  color_used : Map[Int, Int] // color -> freq
  assignment : Map[Int, Int] // reg -> color
  order : Array[Int] // probably the simplicial elimination order
  avail : Int // available colors
  spilled : Map[Int, Int] // spilled registers -> spilled slots
  spilled_colors : Set[Int] // spilled colors
  hints : Map[Int, Set[Int]] // reg -> colors
  uses_count : Map[Int, Int] // reg -> freq
  mut highest_color : Int
}

pub fn PPAllocator::go[Info : Show](self : PPAllocator[Info]) -> Unit {
  self.mcs()
  self.assign_color()
  self.post_spill()
}

pub fn PPAllocator::query_assigned_color[R : RegToId, Info : Show](
  self : PPAllocator[Info],
  reg : R
) -> Info? {
  let color = self.assignment.get(reg.to_reg_id())
  match color {
    Some(c) => self.color_info.get(c)
    None => None
  }
}

pub fn PPAllocator::query_spill[R : RegToId, Info](
  self : PPAllocator[Info],
  reg : R
) -> Int? {
  self.spilled.get(reg.to_reg_id())
}

pub fn PPAllocator::query_all_spilled[Info](
  self : PPAllocator[Info]
) -> Array[Info] {
  self.spilled.iter().map(fn(r) { self.info.get(r.1).unwrap() }).collect()
}

pub fn PPAllocator::query_all_spilled_with_slots[Info](
  self : PPAllocator[Info]
) -> Array[(Info, Int)] {
  self.spilled
  .iter()
  .map(
    fn(r) {
      let (reg, slot) = r
      (self.info.get(reg).unwrap(), slot)
    },
  )
  .collect()
}

pub fn PPAllocator::query_all_clobbered[Info](
  self : PPAllocator[Info]
) -> Array[Info] {
  let clobbered = Set::new()
  for asgn in self.assignment {
    let (reg, color) = asgn
    // an assignment for hardware register is just a constraint
    // so it doesn't necessarily mean that the register is clobbered
    if self.phy.contains(reg) == false {
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
    let hinted_color = self.hints.get(reg)
    let hint = match hinted_color {
      Some(c) => {
        let hint_color_info = c
          .iter()
          .map(fn(c) { self.color_info.get(c).unwrap() })
          .to_string()
        " (hint ★ \{hint_color_info})"
      }
      None => ""
    }
    let count = self.uses_count.get(reg).or(0)
    let count = " (count - \{count})"
    if self.phy.contains(reg) {
      s += "  r\{reg} [label=\"\{info}\", color=blue];\n"
    } else {
      match assigned_color {
        Some(c) =>
          if c >= self.avail {
            s += "  r\{reg} [label=\"\{info} ! spill with color \{c}\{hint}\{count}\", color=orange];\n"
            asgn += "\{info}<-!\{info};"
          } else {
            let color_info = self.color_info.get(c).unwrap()
            s += "  r\{reg} [label=\"\{info} ← \{color_info}\{hint}\{count}\", color=green];\n"
            asgn += "\{info}<-\{color_info};"
          }
        None =>
          s += "  r\{reg} [label=\"\{info} ! spilled\{hint}\{count}\", color=red];\n"
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
    color_used: Map::new(),
    highest_color: -1,
    assignment: Map::new(),
    order: [],
    spilled: Map::new(),
    spilled_colors: Set::new(),
    hints: Map::new(),
    uses_count: Map::new(),
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

fn PPAllocator::add_neighbor_of[Info](
  self : PPAllocator[Info],
  x : Int,
  neighbor : Int
) -> Unit {
  if x != neighbor {
    match self.neighbors.get(x) {
      Some(neighbors) => neighbors.insert(neighbor)
      None => self.neighbors.set(x, Set::of([neighbor]))
    }
  }
}

fn PPAllocator::add_neighbor[Info](
  self : PPAllocator[Info],
  a : Int,
  b : Int
) -> Unit {
  self.add_neighbor_of(a, b)
  self.add_neighbor_of(b, a)
}

fn PPAllocator::prepare_neighbor[Info](
  self : PPAllocator[Info],
  a : Int
) -> Unit {
  match self.neighbors.get(a) {
    Some(_) => ()
    None => self.neighbors.set(a, Set::new())
  }
}

pub fn PPAllocator::add_hint[R : RegToId, Info](
  self : PPAllocator[Info],
  r : R,
  color : Int
) -> Unit {
  match self.hints.get(r.to_reg_id()) {
    Some(hint) => hint.insert(color)
    None => self.hints.set(r.to_reg_id(), Set::of([color]))
  }
}

pub fn PPAllocator::add_use[R : RegToId, Info](
  self : PPAllocator[Info],
  reg : R
) -> Unit {
  let reg_id = reg.to_reg_id()
  self.uses_count.set(reg_id, self.uses_count.get(reg_id).or(0) + 1)
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

type V (Int, Int, Int) derive(Eq, Show) // (weight, count, vertex)

impl Compare for V with compare(self, other) {
  let cmp = self._.0.compare(other._.0)
  if cmp == 0 {
    let cmp = self._.1.compare(other._.1) // prefer the higher count
    if cmp == 0 {
      -self._.2.compare(other._.2) // prefer the lower vertex
    } else {
      cmp
    }
  } else {
    cmp // prefer the higher weight
  }
}

fn mcs[Info](self : PPAllocator[Info]) -> Unit {
  self.order.clear()
  let wt = []
  let remained = Set::new()
  for v in 0..<self.vertex_count {
    wt.push(V((0, self.uses_count.get_or_default(v, 0), v)))
    remained.insert(v)
  }
  let r = self.order
  fn update_wt(us : Set[Int]) {
    for i, v in wt {
      if us.contains(v._.2) {
        wt[i] = V((v._.0 + 1, self.uses_count.get_or_default(v._.1, 0), v._.2))
      }
    }
  }

  for _ in 0..<self.vertex_count {
    wt.sort()
    let (_, _, v) = wt.pop().unwrap()._
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

fn assign_color[Info : Show](self : PPAllocator[Info]) -> Unit {
  let order = self.order
  for r in order {
    if self.phy.contains(r) {
      continue
    }
    let neighbors = self.neighbors.get(r).unwrap()
    let used_colors = self.get_used_color(neighbors)
    fn fallback() {
      let mut candidate = 0
      while true {
        if used_colors.contains(candidate) == false {
          break
        }
        candidate += 1
      }
      candidate
    }

    let candidate = match self.hints.get(r) {
      Some(hints) =>
        for hint in hints {
          if used_colors.contains(hint) == false {
            break hint
          }
        } else {
          fallback()
        }
      None => fallback()
    }
    self.highest_color = @math.maximum(self.highest_color, candidate)
    self.assignment.set(r, candidate)
    self.color_used.set(candidate, self.color_used.get(candidate).or(0) + 1)
  }
}

fn post_spill[Info : Show](self : PPAllocator[Info]) -> Unit {
  let queue : PriotyQueue[ColorFreq] = PriotyQueue::new()
  for c in self.color_used {
    queue.push((c.0, c.1))
  }
  let extra_color = self.highest_color + 1 - self.avail
  let spilled_color = self.spilled_colors
  let spilled_order = []
  for _ in 0..<extra_color {
    let (color, _) = queue.pop().unwrap()._
    self.color_used.remove(color)
    spilled_color.insert(color)
    spilled_order.push(color)
  }
  spilled_order.rev_inplace()
  let color_shift_map = Map::new()
  let mut used_color = 0
  for c in 0..=self.highest_color {
    if spilled_color.contains(c) == false {
      color_shift_map.set(c, used_color)
      used_color += 1
    }
  }
  let asgns = self.assignment.iter().to_array()
  for asgn in asgns {
    let (reg, color) = asgn
    if spilled_color.contains(color) {
      self.spilled.set(reg, spilled_order.search(color).unwrap())
      self.assignment.remove(reg)
    } else {
      let new_color = color_shift_map.get(color).or(color)
      self.assignment.set(reg, new_color)
    }
  }
}
