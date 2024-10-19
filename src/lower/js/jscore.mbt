typealias MutMap[K, V] = @hashmap.T[K, V]

typealias MutSet[K] = @hashset.T[K]

pub struct JsCore {
  entry : JsFn
}

pub struct JsFn {
  name : String
  params : Array[String]
  expr : JsExpr
}

pub enum JsExpr {
  Var(String)
  Lit(String)
  Seq(JsExpr, JsExpr)
  LetRec(JsFn, JsExpr)
  Let(String, JsExpr, JsExpr)
  LetTuple(Array[String], JsExpr, JsExpr)
  Get(JsExpr, JsExpr)
  Put(JsExpr, JsExpr, JsExpr)
  UnOp(String, JsExpr)
  BinOp(String, JsExpr, JsExpr)
  If(JsExpr, JsExpr, JsExpr)
  Call(JsExpr, Array[JsExpr])
  Tuple(Array[JsExpr])
}

pub fn JsCore::output(self : JsCore, logger : Logger) -> Unit {
  let logger = @util.IndentLogger::new(logger)
  self.entry.output(logger)
  logger.write_string("export default \{self.entry.name};\n")
}

pub fn JsFn::output(self : JsFn, logger : @util.IndentLogger) -> Unit {
  logger.write_string("function \{self.name}(")
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
    LetRec(func, body) => {
      func.output(logger)
      body.output_with_tail_return(logger)
    }
    Seq(lhs, rhs) => {
      lhs.output_as_expr(logger)
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
      logger.write_string(";")
      body.output_with_tail_return(logger)
    }
    Get(obj, key) => {
      logger.write_string("return ")
      obj.output_as_expr(logger)
      logger.write_string("[")
      key.output_as_expr(logger)
      logger.write_string("]")
    }
    Put(obj, key, value) => {
      obj.output_as_expr(logger)
      logger.write_string("[")
      key.output_as_expr(logger)
      logger.write_string("] = ")
      value.output_as_expr(logger)
      logger.write_string(";\nreturn null")
    }
    UnOp(op, expr) => {
      logger.write_string("return \{op}")
      expr.output_as_expr(logger)
    }
    BinOp(op, lhs, rhs) => {
      logger.write_string("return ")
      lhs.output_as_expr(logger)
      logger.write_string(" \{op} ")
      rhs.output_as_expr(logger)
    }
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
      logger.write_string("return ")
      name.output_as_expr(logger)
      logger.write_string("(")
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
      let mut first = true
      for expr in exprs {
        if first {
          first = false
        } else {
          logger.write_string(", ")
        }
        expr.output_as_expr(logger)
      }
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
    LetRec(func, body) => {
      func.output(logger)
      body.output_as_expr(logger)
    }
    Seq(lhs, rhs) => {
      lhs.output_as_expr(logger)
      logger.write_string(";\n")
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
    Get(obj, key) => {
      obj.output_as_expr(logger)
      logger.write_string("[")
      key.output_as_expr(logger)
      logger.write_string("]")
    }
    Put(obj, key, value) => {
      obj.output_as_expr(logger)
      logger.write_string("[")
      key.output_as_expr(logger)
      logger.write_string("] = ")
      value.output_as_expr(logger)
    }
    UnOp(op, expr) => {
      logger.write_string("\{op}")
      expr.output_as_expr(logger)
    }
    BinOp(op, lhs, rhs) => {
      lhs.output_as_expr(logger)
      logger.write_string(" \{op} ")
      rhs.output_as_expr(logger)
    }
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
      name.output_as_expr(logger)
      logger.write_string("(")
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
      let mut first = true
      for expr in exprs {
        if first {
          first = false
        } else {
          logger.write_string(", ")
        }
        expr.output_as_expr(logger)
      }
      logger.write_string("]")
    }
  }
}
