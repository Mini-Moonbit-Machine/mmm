pub enum Syntax {
  Unit
  Bool(Bool) // true false
  Int(Int64) // int
  Double(Double) // double
  Var(String) // _
  Tuple(Array[Syntax]) // (_, _, _)
  Not(Syntax) // not(_)
  Array(Syntax, Syntax) // Array::make(_, _)
  Neg(Syntax, mut ~kind : Kind?) // -_
  App(Syntax, Array[Syntax]) // _(_, _, _) or _()
  Get(Syntax, Syntax) // _[_]
  If(Syntax, Syntax, Syntax) // if _ { _ } else { _ }
  Prim(Syntax, Syntax, Op, mut ~kind : Kind?) // _+ - * / _
  Eq(Syntax, Syntax) // _==_
  LE(Syntax, Syntax) // _<=_
  Let((String, Type), Syntax, Syntax) // let _: _ = _; _
  LetRec(Fundef, Syntax) // fn f() {} ; _
  LetTuple(Array[(String, Type)], Syntax, Syntax) // let (_ , _) : (_, _)= _; _
  Put(Syntax, Syntax, Syntax) // _[_] = _
  Match(Syntax, Array[MatchArm]) // match _ { _ => _ }
} derive(Show)

pub struct MatchArm {
  pat: Pattern
  action: Syntax
} derive(Show)

pub enum Pattern {
  Wildcard
  Unit
  Bool(Bool)
  Int(Int)
  Double(Double)
  Var(String)
  Tuple(Array[Pattern])
  CtorApp(Syntax, Array[Pattern])
} derive(Show)

pub enum Kind {
  Int
  Double
} derive(Show, Eq)

pub enum Op {
  Add
  Sub
  Mul
  Div
} derive(Show, Eq)

pub struct Fundef {
  name : (String, Type)
  args : Array[(String, Type)]
  body : Syntax
} derive(Show)

pub enum Type {
  Unit
  Bool
  Int
  Double
  Fun(Array[Type], Type) // (_, _, _) -> _
  Tuple(Array[Type]) // (_, _, _)
  Array(Type) // Array[_]
  Var(Ref[Type?])
  Ptr
} derive(Show)

pub fn Type::is_ptr_like(self : Type) -> Bool {
  match self {
    Fun(_) | Tuple(_) | Array(_) | Ptr => true
    Var(t) =>
      match t.val {
        Some(t) => t.is_ptr_like()
        None => false
      }
    _ => false
  }
}

pub fn Type::is_float_like(self : Type) -> Bool {
  match self {
    Double => true
    Var(t) =>
      match t.val {
        Some(t) => t.is_float_like()
        None => false
      }
    _ => false
  }
}

pub fn Type::is_fn_like(self : Type) -> Bool {
  match self {
    Fun(_, _) => true
    Var(t) =>
      match t.val {
        Some(t) => t.is_fn_like()
        None => false
      }
    _ => false
  }
}

pub fn Type::size_of(self : Type, size_of_ptr : Int) -> Int {
  match self {
    Unit => 0
    Bool => 4
    Int => 4
    Double => 8
    Tuple(_) | Fun(_, _) | Array(_) | Ptr => size_of_ptr
    Var(t) =>
      match t.val {
        Some(t) => t.size_of(size_of_ptr)
        None => abort("Uninstantiated type variable")
      }
  }
}

pub fn Type::is_unknown(self : Type) -> Bool {
  match self {
    Var(t) =>
      match t.val {
        Some(t) => t.is_unknown()
        None => true
      }
    _ => false
  }
}

pub fn Type::repr(self : Type) -> Type {
  match self {
    Type::Var(t) =>
      match t.val {
        Some(t2) => {
          let t3 = t2.repr()
          t.val = Some(t3)
          t3
        }
        None => self
      }
    _ => self
  }
}

pub fn Type::op_equal(self : Type, other : Type) -> Bool {
  match (self, other) {
    (Unit, Unit) => true
    (Bool, Bool) => true
    (Int, Int) => true
    (Double, Double) => true
    (Fun(xs, x), Fun(ys, y)) => xs == ys && x == y
    (Tuple(xs), Tuple(ys)) => xs == ys
    (Array(x), Array(y)) => x == y
    (Ptr, Ptr) => true
    (Var(x), Var(y)) => x.val == y.val
    _ => false
  }
}
