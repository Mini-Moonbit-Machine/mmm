pub enum Expr {
  Unit
  Bool(Bool)
  Int(Int)
  Var(String)
  Tuple(Array[Expr])
  Op(Op, Expr, Expr)
  App(Expr, Array[Expr])
  Get(Expr, Expr)
  If(Expr, Expr, Expr)
  Let(String, Expr, Expr)
  LetRec(Array[Fun], Expr)
  Put(Expr, Expr, Expr)
} derive(Show)

pub enum Op {
  Add
  Sub
  Mul
  Div
  Eq
} derive(Show, Eq)

pub struct Fun {
  name : String
  args : Array[String]
  body : Expr
} derive(Show)
