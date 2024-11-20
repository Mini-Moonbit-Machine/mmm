pub enum Syntax {
  Unit
  Bool(Bool) // true false
  Int(Int64) // int
  Double(Double) // double
  String(String)
  Var(String) // _
  Tuple(Array[Syntax]) // (_, _, _)
  Not(Syntax) // not(_)
  Array(Syntax, Syntax) // Array::make(_, _)
  Neg(Syntax, mut ~kind : Kind?) // -_
  App(Syntax, mut ~targs : Array[(String, Type)]?, Array[Syntax]) // _(_, _, _) or _()
  Get(Syntax, Syntax) // _[_]
  If(Syntax, Syntax, Syntax) // if _ { _ } else { _ }
  Prim(Syntax, Syntax, Op, mut ~kind : Kind?) // _+ - * / _
  Eq(Syntax, Syntax) // _==_
  LE(Syntax, Syntax) // _<=_
  Let((String, Type), Syntax, Syntax) // let _: _ = _; _
  LetRec(Fundef, Syntax) // fn f() {} ; _
  LetTuple(Array[(String, Type)], Syntax, Syntax) // let (_ , _) : (_, _)= _; _
  Put(Syntax, Syntax, Syntax) // _[_] = _
  CtorApp(String, Array[Syntax]) // _(_, _)
  Match(Syntax, Array[MatchArm]) // match _ { _ => _ }
  LetEnum(String, Type, Syntax) // enum {..}; _
  LetClass(Classdef, Syntax) // class {..}; _
  LetInstance(Instancedef, Syntax) // instance {..}; _
} derive(Show)

pub struct MatchArm {
  pat : Pattern
  action : Syntax
} derive(Show)

pub enum Pattern {
  Wildcard
  Unit
  Bool(Bool)
  Int(Int64)
  Double(Double)
  String(String)
  Var(String)
  Tuple(Array[Pattern])
  CtorApp(String, Array[Pattern])
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
  tyvars : Array[(String, String?)]
  name : (String, Type)
  args : Array[(String, Type)]
  body : Syntax
} derive(Show)

pub struct Classdef {
  tyvar : String
  name : String
  methods : Array[(String, Type)]
} derive(Show)

pub struct Instancedef {
  ty : Type
  name : String
  methods : Array[(String, Syntax)]
} derive(Show)

pub enum Type {
  Unit
  Bool
  Int
  Double
  String
  Fun(Array[Type], Type) // (_, _, _) -> _
  Tuple(Array[Type]) // (_, _, _)
  Array(Type) // Array[_]
  Var(Ref[Type?])
  Ptr
  Named(String)
  Enum(String, Ref[Array[(String, Array[Type])]?]) // enum _ {_; _(_, _); }
}

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

pub fn Type::to_scheme_with_tvars(
  self : Type,
  tvars : Array[String]
) -> TypeScheme {
  (tvars, self)
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
    Fun(_) | Tuple(_) | Array(_) | Ptr | Enum(_, _) => true
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
    Enum(_, _) => abort("Enum type is not supported")
    String => abort("String type is not supported")
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

pub struct TypeMemSet {
  mem : Array[(Int, Type)]
  mut counter : Int
}

pub fn TypeMemSet::new() -> TypeMemSet {
  { mem: [], counter: 0 }
}

pub fn TypeMemSet::map(
  self : TypeMemSet,
  t : Type,
  f : (Int?, Type) -> Type
) -> Type {
  fn do() {
    for t2 in self.mem {
      let (n, t2) = t2
      if physical_equal(t, t2) {
        return f(Some(n), t)
      }
    }
    self.mem.push((self.counter, t))
    self.counter += 1
    let t2 = f(None, t)
    t2
  }

  match t {
    Type::Enum(_, _) => do()
    Type::Var(_) => do()
    _ => f(None, t)
  }
}

pub fn TypeMemSet::iter(
  self : TypeMemSet,
  t : Type,
  f : (Int?, Type) -> Unit
) -> Unit {
  fn do() {
    for t2 in self.mem {
      let (n, t2) = t2
      if physical_equal(t, t2) {
        return f(Some(n), t)
      }
    }
    self.mem.push((self.counter, t))
    self.counter += 1
    f(None, t)
  }

  match t {
    Type::Enum(_, _) => do()
    Type::Var(_) => do()
    _ => f(None, t)
  }
}

pub fn Type::op_equal(self : Type, other : Type) -> Bool {
  match (self, other) {
    (Unit, Unit) => true
    (Bool, Bool) => true
    (Int, Int) => true
    (String, String) => true
    (Double, Double) => true
    (Fun(xs, x), Fun(ys, y)) => xs == ys && x == y
    (Tuple(xs), Tuple(ys)) => xs == ys
    (Array(x), Array(y)) => x == y
    (Ptr, Ptr) => true
    (Var(x), Var(y)) => x.val == y.val
    (Named(x), Named(y)) => x == y
    (Enum(x, _), Enum(y, _)) => x == y
    _ => false
  }
}

pub fn Type::to_string(self : Type) -> String {
  Show::to_string(self)
}

pub fn non_rec_name(self : Type) -> String {
  match self {
    Unit => "unit"
    Bool => "bool"
    Int => "int"
    Double => "double"
    String => "string"
    Fun(_, _) => "fun"
    Tuple(_) => "tuple"
    Array(_) => "array"
    Ptr => "ptr"
    Var(_) => "var"
    Named(name) => "named \{name}"
    Enum(name, _) => "enum \{name}"
  }
}

pub fn Type::output(self : Type, logger : Logger) -> Unit {
  let mem = TypeMemSet::new()
  fn aux(rec_id : Int?, self : Type) {
    match rec_id {
      Some(n) => {
        logger.write_string("~\{n}")
        return
      }
      None => ()
    }
    match self {
      Unit => logger.write_string("Unit")
      Bool => logger.write_string("Bool")
      Int => logger.write_string("Int")
      Double => logger.write_string("Double")
      String => logger.write_string("String")
      Fun(args, ret) => {
        logger.write_string("(")
        let mut first = true
        for arg in args {
          if first {
            first = false
          } else {
            logger.write_string(", ")
          }
          aux(None, arg)
        }
        logger.write_string(") -> ")
        aux(None, ret)
      }
      Tuple(xs) => {
        logger.write_string("(")
        let mut first = true
        for x in xs {
          if first {
            first = false
          } else {
            logger.write_string(", ")
          }
          aux(None, x)
        }
        logger.write_string(")")
      }
      Array(x) => {
        logger.write_string("Array[")
        aux(None, x)
        logger.write_string("]")
      }
      Ptr => logger.write_string("Ptr")
      Var(r) =>
        match r.val {
          Some(t) => mem.iter(t, aux)
          None => logger.write_string("Var(None)")
        }
      Named(name) => logger.write_string(name)
      Enum(name, variants) => {
        logger.write_string("Enum \{name} { ")
        match variants.val {
          None => logger.write_string("None")
          Some(variants) => {
            let mut first = true
            for variant in variants {
              let (variant, args) = variant
              if first {
                first = false
              } else {
                logger.write_string(", ")
              }
              logger.write_string("\{variant}(")
              let mut first = true
              for arg in args {
                if first {
                  first = false
                } else {
                  logger.write_string(", ")
                }
                mem.iter(arg, aux)
              }
              logger.write_string(")")
            }
          }
        }
        logger.write_string(" }")
      }
    }
  }

  mem.iter(self, aux)
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
    String(s) => logger.write_string("String(\{s.escape()})")
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
        Let(_) | LetRec(_) | LetTuple(_, _, _) | LetEnum(_, _, _) => {
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
    LetEnum(name, ty, e) => {
      logger.write_string("enum \{name} = \{ty}\n")
      self.print_one_level(logger, e)
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
    App(name, ~targs, args) => {
      logger.write_string("Apply")
      match targs {
        Some(targs) => {
          logger.write_string("[")
          for t in targs {
            logger.write_string(t.to_string())
            logger.write_string(",")
          }
          logger.write_string("]")
        }
        None => ()
      }
      logger.write_string("(\{name}, [")
      for arg in args {
        logger.write_string(arg.to_string())
        logger.write_string(", ")
      }
      logger.write_string("])")
    }
    CtorApp(name, args) => {
      logger.write_string("CtorApp(\{name}, [")
      for arg in args {
        logger.write_string(arg.to_string())
        logger.write_string(", ")
      }
      logger.write_string("])")
    }
    Match(e, arms) => {
      logger.write_string("Match(\{e}, [\n")
      logger.indent()
      for arm in arms {
        let { pat, action } = arm
        logger.write_string("\{pat} =>\n")
        logger.indent()
        self.print_one_level(logger, action)
        logger.outdent()
        logger.write_string("\n")
      }
      logger.outdent()
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
    LetClass({ tyvar, name, methods }, e) => {
      logger.write_string("class \{name} [\{tyvar}] {")
      logger.indent()
      logger.write_string("\n")
      for method in methods {
        let (method_name, method_ty) = method
        logger.write_string("\{method_name}: \{method_ty}\n")
      }
      logger.outdent()
      logger.write_string("}\n")
      self.print_one_level(logger, e)
    }
    LetInstance({ ty, name, methods }, e) => {
      logger.write_string("instance \{name} [\{ty}] {")
      logger.indent()
      logger.write_string("\n")
      for method in methods {
        let (method_name, method_body) = method
        logger.write_string("\{method_name} = ")
        logger.indent()
        self.print_one_level(logger, method_body)
        logger.write_string("\n")
        logger.outdent()
      }
      logger.outdent()
      logger.write_string("}\n")
      self.print_one_level(logger, e)
    }
  }
}

pub impl Show for PrettySyntax with output(self : PrettySyntax, logger : Logger) -> Unit {
  let indent_logger = @util.IndentLogger::new(logger)
  self.print_one_level(indent_logger, self._)
}
