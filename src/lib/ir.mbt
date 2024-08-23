pub enum Node {
  Unit
  Bool(Bool)
  Int(Int)
  Var(String)
  Tuple(Array[Node])
  Op(Op, Node, Node)
  App(Node, Array[Node])
  CApp(String, Array[Node])
  Get(Node, Node)
  If(Node, Node, Node)
  Let(String?, Node, Node)
  LetRec(Array[Defn], Node)
  Put(Node, Node, Node)
} derive(Show)

pub struct Defn {
  name : String
  params : Array[String]
  body : Node
} derive(Show)

typealias FreeVarsSet = @immut/hashset.T[String]

fn union(a : FreeVarsSet, b : FreeVarsSet) -> FreeVarsSet {
  let mut a = a
  for x in b {
    a = a.add(x)
  }
  a
}

fn unions(sets : Array[FreeVarsSet]) -> FreeVarsSet {
  sets.fold_left(fn { acc, x => union(acc, x) }, init=FreeVarsSet::new())
}

pub fn free_var_of_defn(defn : Defn) -> FreeVarsSet {
  let mut x = free_var(defn.body)
  for y in defn.params {
    x = x.remove(y)
  }
  x.remove(defn.name)
}

pub fn free_var(node : Node) -> FreeVarsSet {
  match node {
    Unit => FreeVarsSet::new()
    Bool(_) => FreeVarsSet::new()
    Int(_) => FreeVarsSet::new()
    Var(x) => FreeVarsSet::new().add(x)
    Tuple(xs) => unions(xs.map(free_var))
    Op(_, x, y) => union(free_var(x), free_var(y))
    App(f, xs) => union(free_var(f), unions(xs.map(free_var)))
    CApp(_, xs) => unions(xs.map(free_var))
    Get(x, y) => union(free_var(x), free_var(y))
    If(x, y, z) => union(free_var(x), union(free_var(y), free_var(z)))
    Let(x, y, z) =>
      union(
        free_var(y),
        x.map_or_else(fn() { free_var(z) }, fn(it) { free_var(z).remove(it) }),
      )
    LetRec(fs, x) => {
      let x = free_var(x)
      let names = fs.map(fn { f => f.name })
      let x = fs.fold_left(
        init=x,
        fn { acc, f => union(acc, free_var_of_defn(f)) },
      )
      let x = names.fold_left(init=x, fn { acc, f => acc.remove(f) })
      x
    }
    Put(x, y, z) => union(free_var(x), union(free_var(y), free_var(z)))
  }
}
