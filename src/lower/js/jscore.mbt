typealias MutMap[K, V] = @hashmap.T[K, V]

typealias MutSet[K] = @hashset.T[K]

pub struct JsCore {
  entry : JsFn
}

pub struct JsFn {
  name : String?
  params : Array[String]
  expr : JsExpr
}

pub enum JsExpr {
  Var(String)
  Lit(String)
  Thunk(JsExpr)
  Fn(JsFn)
  LetFn(JsFn, JsExpr)
  LetRec(JsFn, JsExpr)
  Let(String, JsExpr, JsExpr)
  LetTuple(Array[String], JsExpr, JsExpr)
  Get(String, String)
  Put(String, String, String)
  UnOp(String, String)
  BinOp(String, String, String)
  IntBinOp(String, String, String)
  If(JsExpr, JsExpr, JsExpr)
  Call(String, Array[JsExpr])
  Tuple(Array[String])
}

pub fn JsCore::output(self : JsCore, logger : Logger) -> Unit {
  let logger = @util.IndentLogger::new(logger)
  self.entry.output(logger)
  logger.write_string(
    #|
    #|function $thunk(lambda) {
    #|  return { thunk: lambda };
    #|}
    #|function $is_thunk(x) {
    #|  return x && x.hasOwnProperty('thunk') && x.thunk !== undefined;
    #|}
    #|function $force(x) {
    #|  while ($is_thunk(x)) {
    #|    x = x.thunk();
    #|  }
    #|  return x;
    #|}
    #|function $external(f, k, ...args) {
    #|  let x = f(...args);
    #|  return $thunk(() => k(x));
    #|}
    #|function $trampoline(f, ...args) {
    #|  let x = f(...args);
    #|  return $force(x);
    #|}
    #|
    ,
  )
  logger.write_string(
    "export default () => $trampoline(\{self.entry.name.unwrap()}, x => x);",
  )
}

pub fn JsFn::output(self : JsFn, logger : @util.IndentLogger) -> Unit {
  match self.name {
    Some(name) => logger.write_string("function \{name}(")
    None => logger.write_string("function (")
  }
  let mut first = true
  for param in self.params {
    if first {
      first = false
    } else {
      logger.write_string(", ")
    }
    logger.write_string(param)
  }
  logger.write_string(") {\n")
  logger.indent()
  self.expr.output_with_tail_return(logger)
  logger.write_string("\n")
  logger.outdent()
  logger.write_string("}\n")
}

pub fn JsExpr::output_with_tail_return(
  self : JsExpr,
  logger : @util.IndentLogger
) -> Unit {
  match self {
    JsExpr::Var(name) => logger.write_string("return \{name}")
    Lit(lit) => logger.write_string("return \{lit}")
    Thunk(func) => {
      logger.write_string("return $thunk(() => ")
      func.output_as_expr(logger)
      logger.write_string(")")
    }
    Fn(func) => {
      logger.write_string("return ")
      func.output(logger)
    }
    LetFn(func, body) => {
      func.output(logger)
      body.output_with_tail_return(logger)
    }
    LetRec(lhs, rhs) => {
      lhs.output(logger)
      rhs.output_with_tail_return(logger)
    }
    Let(name, value, body) => {
      logger.write_string("const \{name} = ")
      value.output_as_expr(logger)
      logger.write_string(";\n")
      body.output_with_tail_return(logger)
    }
    LetTuple(names, tuple, body) => {
      let destruct = names.join(", ")
      logger.write_string("const [\{destruct}] = ")
      tuple.output_as_expr(logger)
      logger.write_string(";\n")
      body.output_with_tail_return(logger)
    }
    Get(obj, key) => logger.write_string("return \{obj}[\{key}]")
    Put(obj, key, value) =>
      logger.write_string("\{obj}[\{key}] = \{value};\nreturn null;")
    UnOp(op, expr) => logger.write_string("return \{op}\{expr};")
    BinOp(op, lhs, rhs) => logger.write_string("return \{lhs} \{op} \{rhs};")
    IntBinOp(op, lhs, rhs) =>
      logger.write_string("return ~~(\{lhs} \{op} \{rhs});")
    If(cond, then, els) => {
      logger.write_string("if (")
      cond.output_as_expr(logger)
      logger.write_string(") {\n")
      logger.indent()
      then.output_with_tail_return(logger)
      logger.outdent()
      logger.write_string("\n} else {\n")
      logger.indent()
      els.output_with_tail_return(logger)
      logger.outdent()
      logger.write_string("\n}")
    }
    Call(name, args) => {
      logger.write_string("return \{name}(")
      let mut first = true
      for arg in args {
        if first {
          first = false
        } else {
          logger.write_string(", ")
        }
        arg.output_as_expr(logger)
      }
      logger.write_string(")")
    }
    Tuple(exprs) => {
      logger.write_string("return [")
      logger.write_string(exprs.join(", "))
      logger.write_string("]")
    }
  }
}

pub fn JsExpr::output_as_expr(
  self : JsExpr,
  logger : @util.IndentLogger
) -> Unit {
  match self {
    JsExpr::Var(name) => logger.write_string(name)
    Lit(lit) => logger.write_string(lit)
    Thunk(func) => {
      logger.write_string("$thunk(() => ")
      func.output_as_expr(logger)
      logger.write_string(")")
    }
    Fn(func) => func.output(logger)
    LetFn(func, body) => {
      func.output(logger)
      body.output_as_expr(logger)
    }
    LetRec(lhs, rhs) => {
      lhs.output(logger)
      rhs.output_as_expr(logger)
    }
    Let(name, value, body) => {
      logger.write_string("const \{name} = ")
      value.output_as_expr(logger)
      logger.write_string(";\n")
      body.output_as_expr(logger)
    }
    LetTuple(names, tuple, body) => {
      let destruct = names.join(", ")
      logger.write_string("const [\{destruct}] = ")
      tuple.output_as_expr(logger)
      logger.write_string(";")
      body.output_as_expr(logger)
    }
    Get(obj, key) => logger.write_string("\{obj}[\{key}]")
    Put(obj, key, value) => logger.write_string("\{obj}[\{key}] = \{value}")
    UnOp(op, expr) => logger.write_string("\{op}\{expr}")
    BinOp(op, lhs, rhs) => logger.write_string("\{lhs} \{op} \{rhs}")
    IntBinOp(op, lhs, rhs) => logger.write_string("~~(\{lhs} \{op} \{rhs})")
    If(cond, then, els) => {
      logger.write_string("(() => {\n")
      logger.indent()
      logger.write_string("if (")
      cond.output_as_expr(logger)
      logger.write_string(") {\n")
      logger.indent()
      then.output_with_tail_return(logger)
      logger.outdent()
      logger.write_string("\n} else {\n")
      logger.indent()
      els.output_with_tail_return(logger)
      logger.outdent()
      logger.write_string("\n}")
      logger.outdent()
      logger.write_string("\n})()")
    }
    Call(name, args) => {
      logger.write_string("\{name}(")
      let mut first = true
      for arg in args {
        if first {
          first = false
        } else {
          logger.write_string(", ")
        }
        arg.output_as_expr(logger)
      }
      logger.write_string(")")
    }
    Tuple(exprs) => {
      logger.write_string("[")
      logger.write_string(exprs.join(", "))
      logger.write_string("]")
    }
  }
}
