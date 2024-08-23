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

struct IrBuilder {
  mut fresh : Int
}

typealias LetList = Array[(String?, Node)]

fn IrBuilder::new() -> IrBuilder {
  { fresh: 0 }
}

fn next_fresh(self : IrBuilder) -> String {
  let x = self.fresh
  self.fresh = x + 1
  "x" + x.to_string()
}

fn let_list_to_anf(ls : LetList, node : Node) -> Node {
  let mut node = node
  for binding in ls {
    let (x, y) = binding
    node = Let(x, y, node)
  }
  node
}

fn defn_to_anf(fun : Fun) -> Defn {
  let ls = Array::new()
  let body = expr_to_anf(ls, fun.body)
  { name: fun.name, params: fun.params, body: let_list_to_anf(ls, body) }
}

fn to_anf(expr : Expr) -> Node {
  let ls = Array::new()
  expr_to_anf(ls, expr)
}

fn expr_to_anf(ls : LetList, expr : Expr) -> Node {
  match expr {
    Unit => Unit
    Bool(x) => Bool(x)
    Int(x) => Int(x)
    Var(x) => Var(x)
    Tuple(xs) => {
      let ys = Array::new()
      for x in xs {
        ys.push(expr_to_anf(ls, x))
      }
      Tuple(ys)
    }
    Op(op, x, y) => {
      let x = expr_to_anf(ls, x)
      let y = expr_to_anf(ls, y)
      Op(op, x, y)
    }
    App(f, xs) => {
      let f = expr_to_anf(ls, f)
      let ys = Array::new()
      for x in xs {
        ys.push(expr_to_anf(ls, x))
      }
      App(f, ys)
    }
    Get(x, y) => {
      let x = expr_to_anf(ls, x)
      let y = expr_to_anf(ls, y)
      Get(x, y)
    }
    If(x, y, z) => {
      let x = expr_to_anf(ls, x)
      let ls1 = Array::new()
      let y = expr_to_anf(ls1, y)
      let ls2 = Array::new()
      let z = expr_to_anf(ls2, z)
      If(x, let_list_to_anf(ls1, y), let_list_to_anf(ls2, z))
    }
    Let("_", x, y) => {
      let x = expr_to_anf(ls, x)
      ls.push((None, x))
      expr_to_anf(ls, y)
    }
    Let(x, y, z) => {
      let x = x
      let y = expr_to_anf(ls, y)
      ls.push((Some(x), y))
      expr_to_anf(ls, z)
    }
    LetRec(fs, x) => {
      let fs = fs.map(defn_to_anf)
      let x = expr_to_anf(ls, x)
      LetRec(fs, x)
    }
    Put(x, y, z) => {
      let x = expr_to_anf(ls, x)
      let y = expr_to_anf(ls, y)
      let z = expr_to_anf(ls, z)
      Put(x, y, z)
    }
  }
}

typealias RenameCtx = @immut/hashmap.T[String, String]

fn alpha_rename_defn(self : IrBuilder, ctx : RenameCtx, defn : Defn) -> Defn {
  let ctx = defn.params.fold_left(init=ctx, fn { acc, x => acc.add(x, x) })
  let ctx = ctx.add(defn.name, defn.name)
  let body = alpha_rename_with_ctx(self, ctx, defn.body)
  { name: defn.name, params: defn.params, body }
}

fn alpha_rename(self : IrBuilder, node : Node) -> Node {
  alpha_rename_with_ctx(self, RenameCtx::new(), node)
}

fn alpha_rename_with_ctx(
  self : IrBuilder,
  ctx : RenameCtx,
  node : Node
) -> Node {
  match node {
    Unit => Unit
    Bool(x) => Bool(x)
    Int(x) => Int(x)
    Var(x) => Var(ctx[x].unwrap())
    Tuple(xs) => Tuple(xs.map(fn { x => alpha_rename_with_ctx(self, ctx, x) }))
    Op(op, x, y) =>
      Op(
        op,
        alpha_rename_with_ctx(self, ctx, x),
        alpha_rename_with_ctx(self, ctx, y),
      )
    App(f, xs) =>
      App(
        alpha_rename_with_ctx(self, ctx, f),
        xs.map(fn { x => alpha_rename_with_ctx(self, ctx, x) }),
      )
    CApp(f, xs) =>
      CApp(f, xs.map(fn { x => alpha_rename_with_ctx(self, ctx, x) }))
    Get(x, y) =>
      Get(
        alpha_rename_with_ctx(self, ctx, x),
        alpha_rename_with_ctx(self, ctx, y),
      )
    If(x, y, z) =>
      If(
        alpha_rename_with_ctx(self, ctx, x),
        alpha_rename_with_ctx(self, ctx, y),
        alpha_rename_with_ctx(self, ctx, z),
      )
    Let(None, x, y) =>
      Let(
        None,
        alpha_rename_with_ctx(self, ctx, x),
        alpha_rename_with_ctx(self, ctx, y),
      )
    Let(Some(x), y, z) => {
      let x2 = self.next_fresh()
      Let(
        Some(x),
        alpha_rename_with_ctx(self, ctx, y),
        alpha_rename_with_ctx(self, ctx.add(x, x2), z),
      )
    }
    LetRec(fs, x) => {
      let names = fs.map(fn { f => f.name })
      let ctx = names.fold_left(init=ctx, fn { acc, f => acc.add(f, f) })
      let fs = fs.map(fn { f => alpha_rename_defn(self, ctx, f) })
      LetRec(fs, alpha_rename_with_ctx(self, ctx, x))
    }
    Put(x, y, z) =>
      Put(
        alpha_rename_with_ctx(self, ctx, x),
        alpha_rename_with_ctx(self, ctx, y),
        alpha_rename_with_ctx(self, ctx, z),
      )
  }
}
