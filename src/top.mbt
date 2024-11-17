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
  tyvars : Array[String]
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
  Named(String)
  Ptr
} derive(Show)

pub type TypeScheme (Array[String], Type) derive(Show)

pub fn Type::new_var() -> Type {
  Type::Var(Ref::new(None))
}

pub fn Type::new_named(name : String) -> Type {
  Type::Named(name)
}

pub fn Type::to_scheme(self : Type) -> TypeScheme {
  ([], self)
}

pub fn TypeScheme::no_quantifier(self : TypeScheme) -> Type? {
  if self._.0.length() == 0 {
    Some(self._.1)
  } else {
    None
  }
}

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
    Named(_) => 8
  }
}

pub fn Type::is_unknown(self : Type) -> Bool {
  match self {
    Var(t) =>
      match t.val {
        Some(t) => t.is_unknown()
        None => true
      }
    Named(_) => true
    _ => false
  }
}

pub fn Type::is_uninstantiated(self : Type) -> Bool {
  match self {
    Var(t) =>
      match t.val {
        Some(t) => t.is_uninstantiated()
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

pub fn Type::map_named(self : Type, m : (String) -> Type) -> Type {
  fn aux(t : Type) {
    let repr = t.repr()
    match repr {
      Named(name) => m(name)
      Fun(args, ret) => Type::Fun(args.map(aux), aux(ret))
      Tuple(xs) => Tuple(xs.map(aux))
      Array(x) => Array(aux(x))
      _ => repr
    }
  }

  aux(self)
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
    (Named(x), Named(y)) => x == y
    _ => false
  }
}

pub type PrettySyntax Syntax

pub fn Syntax::to_pretty_print(self : Syntax) -> PrettySyntax {
  self
}

fn PrettySyntax::print_one_level(
  self : PrettySyntax,
  logger : @util.IndentLogger,
  val : Syntax
) -> Unit {
  match val {
    Unit => logger.write_string("Unit")
    Bool(b) => logger.write_string("Bool(\{b})")
    Int(i) => logger.write_string("Int(\{i})")
    Double(d) => logger.write_string("Double(\{d})")
    Not(e) => logger.write_string("Not(\{e})")
    Neg(e, ..) => logger.write_string("Neg(\{e})")
    Var(name) => logger.write_string("Var(\{name})")
    Prim(e1, e2, op, ..) => {
      let op_str = match op {
        Add => "+"
        Sub => "-"
        Mul => "*"
        Div => "/"
      }
      logger.write_string("\{e1} \{op_str} \{e2}")
    }
    Eq(e1, e2) => logger.write_string("\{e1} == \{e2}")
    LE(e1, e2) => logger.write_string("\{e1} <= \{e2}")
    Array(e1, e2) => logger.write_string("Array::make(\{e1}, \{e2})")
    If(e1, e3, e4) => {
      logger.write_string("if (\{e1}) then {")
      logger.indent()
      logger.write_string("\n")
      self.print_one_level(logger, e3)
      logger.outdent()
      logger.write_string("\n")
      logger.write_string("} else {")
      logger.indent()
      logger.write_string("\n")
      self.print_one_level(logger, e4)
      logger.outdent()
      logger.write_string("\n}")
    }
    Let((name, ty), e1, e2) => {
      logger.write_string("let \{name}: \{ty} = ")
      match e1 {
        Let(_) | LetRec(_) | LetTuple(_, _, _) => {
          logger.write_string("\n")
          logger.indent()
          self.print_one_level(logger, e1)
          logger.outdent()
        }
        _ => self.print_one_level(logger, e1)
      }
      logger.write_string("\n")
      self.print_one_level(logger, e2)
    }
    LetTuple(name_types, y, e) => {
      logger.write_string("let (")
      let mut first = true
      for nt in name_types {
        if first {
          first = false
        } else {
          logger.write_string(", ")
        }
        let (name, ty) = nt
        logger.write_string("\{name}: \{ty}")
      }
      logger.write_string(") = \{y}\n")
      self.print_one_level(logger, e)
    }
    LetRec(fundef, e) => {
      let { name, args, body, tyvars } = fundef
      let quantifier = if tyvars.length() == 0 {
        ""
      } else {
        "forall \{tyvars} . "
      }
      logger.write_string("let rec \{name.0}: \{quantifier}(")
      let mut first = true
      for arg in args {
        let (arg_name, arg_ty) = arg
        if first {
          first = false
        } else {
          logger.write_string(", ")
        }
        logger.write_string("\{arg_name}: \{arg_ty}")
      }
      logger.write_string(") -> \{name.1} {")
      logger.indent()
      logger.write_string("\n")
      self.print_one_level(logger, body)
      logger.outdent()
      logger.write_string("\n}\n")
      self.print_one_level(logger, e)
    }
    App(name, args) => {
      logger.write_string("Apply(\{name}, [")
      for arg in args {
        logger.write_string(arg.to_string())
        logger.write_string(", ")
      }
      logger.write_string("])")
    }
    Tuple(es) => {
      logger.write_string("Tuple([")
      for e in es {
        logger.write_string(e.to_string())
        logger.write_string(", ")
      }
      logger.write_string("])")
    }
    Put(e1, e2, e3) => logger.write_string("Put \{e1}[\{e2}] = \{e3}")
    Get(e1, e2) => logger.write_string("Get \{e1}[\{e2}]")
  }
}

pub impl Show for PrettySyntax with output(self : PrettySyntax, logger : Logger) -> Unit {
  let indent_logger = @util.IndentLogger::new(logger)
  self.print_one_level(indent_logger, self._)
}
