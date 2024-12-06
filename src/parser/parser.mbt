typealias Lexer = @lexer.Lexer

typealias Token = @lexer.Token

typealias TokenTag = @lexer.TokenTag

typealias Paired = @lexer.Paired

typealias Keyword = @lexer.Keyword

typealias Type = @types.Type

typealias Syntax = @types.Syntax

typealias Pattern = @types.Pattern

typealias MatchArm = @types.MatchArm

typealias Op = @types.Op

struct UserOp {
  assoc_left : Bool
  prec : Int
  func : String
}

pub struct Parser {
  tokenizer : Lexer
  tracer : @util.SubTracer
  op : Map[String, UserOp]
  mut token : Token?
}

pub struct PResult[T] {
  inner : T
  ntokens : Int
}

typealias PToken = PResult[Token]

typealias PType = PResult[Type]

typealias PSyntax = PResult[Syntax]

fn Parser::level_none() -> Int {
  0x7fffffff
}

fn Parser::postfix_start() -> Int {
  3000
}

fn Parser::infix_start() -> Int {
  2000
}

fn Parser::prefix_start() -> Int {
  1000
}

fn Parser::prefix(self : Parser, op : String) -> (Int, Int)? {
  let _ = self
  match op {
    "-" => Some((Parser::level_none(), Parser::prefix_start() + 1))
    _ => None
  }
}

fn Parser::postfix(self : Parser, op : String) -> (Int, Int)? {
  let _ = self
  match op {
    "(" => Some((Parser::postfix_start() + 1, Parser::level_none()))
    "[" => Some((Parser::postfix_start() + 1, Parser::level_none()))
    _ => None
  }
}

fn Parser::infix(self : Parser, op : String) -> (Int, Int)? {
  match op {
    "<=" => Some((Parser::infix_start() + 1, Parser::infix_start() + 2))
    "==" => Some((Parser::infix_start() + 1, Parser::infix_start() + 2))
    "+" => Some((Parser::infix_start() + 3, Parser::infix_start() + 4))
    "-" => Some((Parser::infix_start() + 3, Parser::infix_start() + 4))
    "*" => Some((Parser::infix_start() + 5, Parser::infix_start() + 6))
    "/" => Some((Parser::infix_start() + 5, Parser::infix_start() + 6))
    _ =>
      match self.op.get(op) {
        Some({ assoc_left, prec, .. }) =>
          if assoc_left {
            let prec = prec + Parser::infix_start()
            Some((prec, prec + 1))
          } else {
            let prec = prec + Parser::infix_start()
            Some((prec + 1, prec))
          }
        None => None
      }
  }
}

fn Parser::type_postfix(self : Parser, op : String) -> (Int, Int)? {
  let _ = self
  match op {
    "(" => Some((Parser::postfix_start() + 1, Parser::level_none()))
    "[" => Some((Parser::postfix_start() + 1, Parser::level_none()))
    _ => None
  }
}

fn Parser::type_infix(self : Parser, op : String) -> (Int, Int)? {
  let _ = self
  match op {
    "->" => Some((Parser::infix_start() + 2, Parser::infix_start() + 1))
    _ => None
  }
}

pub fn Parser::new(input : String, tracer : @util.SubTracer) -> Parser {
  { tokenizer: Lexer::new(input), tracer, token: None, op: {} }
}

pub fn Parser::get_flags(self : Parser) -> Array[String] {
  let flags = self.tokenizer.flags.iter().map(fn(flag) { flag.span }).collect()
  self.tracer.log("flags: \{flags}")
  flags
}

fn Parser::skip_token(self : Parser) -> Unit {
  // if it's eof, don't skip
  match self.token {
    Some({ tag: TokenTag::Eof, .. }) => self.tracer.abort("trying to skip eof")
    _ => self.token = None
  }
}

fn Parser::peek_token(self : Parser) -> PToken!Failure {
  let token = match self.token {
    Some(tok) => { inner: tok, ntokens: 1 }
    None => {
      let mut tok = self.tokenizer.next_err!()
      while tok.tag == TokenTag::Newline {
        tok = self.tokenizer.next_err!()
      }
      self.token = Some(tok)
      { inner: tok, ntokens: 1 }
    }
  }
  token
}

pub fn Parser::next_token(self : Parser) -> PToken!Failure {
  match self.token {
    Some(tok) => {
      self.token = None
      { inner: tok, ntokens: 1 }
    }
    None => {
      let mut tok = self.tokenizer.next_err!()
      while tok.tag == TokenTag::Newline {
        tok = self.tokenizer.next_err!()
      }
      { inner: tok, ntokens: 1 }
    }
  }
}

fn Parser::reach_eof(self : Parser) -> Bool {
  try {
    match self.peek_token!() {
      { inner: { tag: TokenTag::Eof, .. }, .. } => true
      _ => false
    }
  } catch {
    _ => false
  }
}

fn Parser::expect_paired_open(self : Parser, po : Paired) -> PToken!Failure {
  match self.next_token!() {
    { inner: { tag: TokenTag::PairedOpen(po2), .. }, .. } as r =>
      if po == po2 {
        r
      } else {
        fail!("expected paired open \{po}, got \{r.inner}")
      }
    tok => fail!("expected paired open \{po}, got \{tok.inner}")
  }
}

fn Parser::expect_paired_close(self : Parser, pc : Paired) -> PToken!Failure {
  match self.next_token!() {
    { inner: { tag: TokenTag::PairedClose(pc2), .. }, .. } as r =>
      if pc == pc2 {
        r
      } else {
        fail!("expected paired close \{pc}, got \{r.inner}")
      }
    tok => fail!("expected paired close \{pc}, got \{tok.inner}")
  }
}

fn Parser::peek_keyword(self : Parser, kw : Keyword) -> Bool {
  let token = try {
    self.peek_token!()
  } catch {
    _ => return false
  }
  match token {
    { inner: { tag: TokenTag::Kw(kw2), .. }, .. } => kw == kw2
    _ => false
  }
}

fn Parser::peek_operator(self : Parser, op : String) -> Bool {
  let token = try {
    self.peek_token!()
  } catch {
    _ => return false
  }
  match token {
    { inner: { tag: TokenTag::Op(op2), .. }, .. } => op == op2
    _ => false
  }
}

fn Parser::expect_keyword(self : Parser, kw : Keyword) -> PToken!Failure {
  if self.peek_keyword(kw) {
    self.next_token!()
  } else {
    fail!("expected keyword \{kw}")
  }
}

fn Parser::expect_operator(self : Parser, op : String) -> PToken!Failure {
  if self.peek_operator(op) {
    self.next_token!()
  } else {
    fail!("expected operator \{op}")
  }
}

fn Parser::expect_ident(self : Parser) -> PToken!Failure {
  match self.next_token!() {
    { inner: { tag: TokenTag::Identifer, .. }, .. } as r => r
    tok => fail!("expected identifier, got \{tok.inner}")
  }
}

fn Parser::expect_int(self : Parser) -> PResult[Int64]!Failure {
  match self.next_token!() {
    { inner: { tag: TokenTag::IntLiteral(n), .. }, .. } =>
      { inner: n, ntokens: 1 }
    tok => fail!("expected int, got \{tok.inner}")
  }
}

pub fn Parser::parse_type(self : Parser) -> PType!Failure {
  self.parse_type_with_level!(0)
}

fn Parser::parse_type_with_level(self : Parser, level : Int) -> PType!Failure {
  let lhs_token = self.next_token!()
  let mut ntokens = lhs_token.ntokens
  let mut lhs = match lhs_token.inner.tag {
    Identifer =>
      match lhs_token.inner.span {
        "Unit" => Type::Unit
        "Int" => Type::Int
        "Double" => Type::Double
        "Bool" => Type::Bool
        "Array" => Type::Array(Type::Unit)
        "String" => Type::String
        ty =>
          if ty[0] >= 'A' && ty[0] <= 'Z' {
            Type::Named(ty)
          } else {
            fail!("unexpected type identifier \{ty}")
          }
      }
    PairedOpen(po) =>
      if self.peek_token!().inner.tag == PairedClose(po) {
        self.skip_token()
        ntokens += 1
        Type::Tuple([])
      } else {
        let ty = self.parse_type!()
        ntokens += ty.ntokens
        if self.peek_operator(",") {
          let tys = [ty.inner]
          while self.peek_operator(",") {
            self.skip_token()
            ntokens += 1
            let ty = self.parse_type!()
            ntokens += ty.ntokens
            tys.push(ty.inner)
          }
          let _ = self.expect_paired_close!(po)
          ntokens += 1
          Type::Tuple(tys)
        } else {
          let _ = self.expect_paired_close!(po)
          ntokens += 1
          ty.inner
        }
      }
    _ => fail!("unexpected token when parsing type: \{lhs_token.inner}")
  }
  while true {
    let op_token = self.peek_token!()
    let op_str = match op_token.inner.tag {
      Op(op) => op
      PairedOpen(po) =>
        match po {
          Paired::Parenthesis => "("
          Paired::Bracket => "["
          Paired::Brace => "{"
        }
      PairedClose(_) => break
      Eof | Newline => break
      Identifer => break
      StrLiteral(_) => break
      IntLiteral(_) => break
      DoubleLiteral(_) => break
      _ => fail!("unexpected token when parsing type")
    }
    match self.type_postfix(op_str) {
      Some((laff, _)) => {
        if laff < level {
          break
        }
        self.skip_token()
        ntokens += 1
        match op_token.inner.tag {
          PairedOpen(Bracket) => {
            let ty = self.parse_type!()
            ntokens += ty.ntokens
            let _ = self.expect_paired_close!(Bracket)
            ntokens += 1
            lhs = match lhs {
              Array(_) => Type::Array(ty.inner)
              _ => fail!("expect left hand side to be Array")
            }
          }
          _ => fail!("unexpected token when parsing type")
        }
        continue
      }
      None => ()
    }
    match self.type_infix(op_str) {
      Some((laff, raff)) => {
        if laff < level {
          break
        }
        if self.peek_operator("->") {
          self.skip_token()
          ntokens += 1
          let rhs = self.parse_type_with_level!(raff)
          ntokens += rhs.ntokens
          match lhs {
            Type::Tuple(inner) => lhs = Type::Fun(inner, rhs.inner)
            _ => lhs = Type::Fun([lhs], rhs.inner)
          }
        }
        continue
      }
      None => ()
    }
    break
  }
  { inner: lhs, ntokens }
}

pub fn Parser::parse_expr(self : Parser) -> PSyntax!Failure {
  self.parse_expr_with_level!(0)
}

pub fn Parser::parse_expr_with_level(
  self : Parser,
  level : Int
) -> PSyntax!Failure {
  let lhs_token = self.next_token!()
  let mut ntokens = lhs_token.ntokens
  let mut lhs = match lhs_token.inner.tag {
    IntLiteral(n) => Syntax::Int(n.to_uint64().to_int64())
    DoubleLiteral(n) => Syntax::Double(n)
    StrLiteral(s) => Syntax::String(s)
    Identifer =>
      match lhs_token.inner.span {
        "true" => Syntax::Bool(true)
        "false" => Syntax::Bool(false)
        "Array" => {
          let _ = self.expect_operator!("::")
          ntokens += 1
          let make = self.next_token!()
          ntokens += 1
          if make.inner.span != "make" {
            fail!("expected Array::make")
          }
          Syntax::Var("<Array::make>")
        }
        x =>
          if x[0] >= 'A' && x[0] <= 'Z' {
            Syntax::CtorApp(x, [])
          } else {
            Syntax::Var(x)
          }
      }
    PairedOpen(po) =>
      if self.peek_token!().inner.tag == PairedClose(po) &&
        po == Paired::Parenthesis {
        self.skip_token()
        ntokens += 1
        Syntax::Unit
      } else if po == Paired::Parenthesis {
        let expr = self.parse_expr!()
        ntokens += expr.ntokens
        if self.peek_operator(",") {
          let exprs = [expr.inner]
          while self.peek_operator(",") {
            self.skip_token()
            ntokens += 1
            let expr = self.parse_expr!()
            ntokens += expr.ntokens
            exprs.push(expr.inner)
          }
          let _ = self.expect_paired_close!(po)
          ntokens += 1
          Syntax::Tuple(exprs)
        } else {
          let _ = self.expect_paired_close!(po)
          ntokens += 1
          expr.inner
        }
      } else if po == Paired::Brace {
        let block = self.parse_stmt!(false)
        ntokens += block.ntokens
        let _ = self.expect_paired_close!(Paired::Brace)
        ntokens += 1
        block.inner
      } else if po == Paired::Bracket {
        // parse array literal
        let mut lookahead = self.peek_token!()
        // empty array case
        match lookahead.inner.tag {
          PairedClose(Bracket) => {
            // empty array
            self.skip_token()
            ntokens += 1
            fail!("empty array not supported")
          }
          _ => {
            // non-empty array
            let exprs = []
            let expr = self.parse_expr!()
            ntokens += expr.ntokens
            exprs.push(expr.inner)
            lookahead = self.peek_token!()
            while lookahead.inner.tag != PairedClose(Bracket) {
              let _ = self.expect_operator!(",")
              ntokens += 1
              let expr = self.parse_expr!()
              ntokens += expr.ntokens
              exprs.push(expr.inner)
              lookahead = self.peek_token!()
            }
            let _ = self.expect_paired_close!(Bracket)
            ntokens += 1
            let arr_name = "_array"
            let make_arr = Syntax::Array(
              Syntax::Int(exprs.length().to_int64()),
              exprs[0],
            )
            let puts = []
            for i = 1; i < exprs.length(); i = i + 1 {
              puts.push(
                Syntax::Put(
                  Syntax::Var(arr_name),
                  Syntax::Int(i.to_int64()),
                  exprs[i],
                ),
              )
            }
            let mut desugar = Syntax::Var(arr_name)
            for put in puts.rev() {
              desugar = Syntax::Let(("_", Type::new_var()), put, desugar)
            }
            desugar = Syntax::Let(
              (arr_name, Type::new_var()),
              make_arr,
              desugar,
            )
            desugar
          }
        }
      } else {
        fail!("unexpected paired when parsing expr: \{lhs_token.inner}")
      }
    Kw(kw) =>
      match kw {
        Keyword::If => {
          let cond = self.parse_expr!()
          ntokens += cond.ntokens
          let then = self.parse_expr!()
          ntokens += then.ntokens
          if self.peek_keyword(Keyword::Else) {
            self.skip_token()
            ntokens += 1
            let els = self.parse_expr!()
            ntokens += els.ntokens
            Syntax::If(cond.inner, then.inner, els.inner)
          } else {
            Syntax::If(cond.inner, then.inner, Syntax::Unit)
          }
        }
        Keyword::Match => {
          let scrutinee = self.parse_expr!()
          ntokens += scrutinee.ntokens
          // parse match arms
          fn syntax_to_pattern(node : Syntax) -> Pattern!Failure {
            match node {
              Syntax::Var("_") => Pattern::Wildcard
              Syntax::Unit => Pattern::Unit
              Syntax::Bool(b) => Pattern::Bool(b)
              Syntax::Int(n) => Pattern::Int(n)
              Syntax::Double(n) => Pattern::Double(n)
              Syntax::String(s) => Pattern::String(s)
              Syntax::Var(name) => Pattern::Var(name)
              Syntax::Tuple(exprs) => {
                let ts = []
                for expr in exprs {
                  ts.push(syntax_to_pattern!(expr))
                }
                Pattern::Tuple(ts)
              }
              Syntax::CtorApp(ctor, exprs) => {
                let args = []
                for expr in exprs {
                  args.push(syntax_to_pattern!(expr))
                }
                Pattern::CtorApp(ctor, args)
              }
              _ => fail!("unexpected syntax in pattern")
            }
          }

          let _ = self.expect_paired_open!(Paired::Brace)
          ntokens += 1
          let match_arms = []
          while self.peek_token!().inner.tag != PairedClose(Brace) {
            let pat_expr = self.parse_expr!()
            ntokens += pat_expr.ntokens
            let pat = syntax_to_pattern!(pat_expr.inner)
            let _ = self.expect_operator!("=>")
            ntokens += 1
            let action_expr = self.parse_expr!()
            ntokens += action_expr.ntokens
            let action = action_expr.inner
            let _ = self.expect_operator!(";")
            ntokens += 1
            match_arms.push(@types.MatchArm::{ pat, action })
          }
          self.skip_token()
          ntokens += 1
          Syntax::Match(scrutinee.inner, match_arms)
        }
        _ => fail!("keyword not supported in expression: \{kw}")
      }
    Op(op) =>
      match self.prefix(op) {
        Some((_, raff)) => {
          let rhs = self.parse_expr_with_level!(raff)
          ntokens += rhs.ntokens
          match op {
            "-" => Syntax::Neg(rhs.inner, kind=None)
            _ => fail!("unexpected prefix operator \{op} in expression")
          }
        }
        None => fail!("unexpected prefix operator \{op} in expression")
      }
    _ => fail!("unexpected token when parsing expr: \{lhs_token.inner}")
  }
  while true {
    let op_token = self.peek_token!()
    let op_str = match op_token.inner.tag {
      Op(op) => op
      PairedOpen(po) =>
        match po {
          Paired::Parenthesis => "("
          Paired::Bracket => "["
          Paired::Brace => break
        }
      PairedClose(_) => break
      Eof | Newline => break
      Kw(_) => break
      _ =>
        fail!("unexpected trailing token when parsing expr \{op_token.inner}")
    }
    match self.postfix(op_str) {
      Some((laff, _)) => {
        if laff < level {
          break
        }
        self.skip_token()
        ntokens += 1
        match op_token.inner.tag {
          PairedOpen(Bracket) => {
            let expr = self.parse_expr!()
            ntokens += expr.ntokens
            let _ = self.expect_paired_close!(Bracket)
            ntokens += 1
            if self.peek_operator("=") {
              self.skip_token()
              ntokens += 1
              let rhs = self.parse_expr!()
              ntokens += rhs.ntokens
              lhs = Syntax::Put(lhs, expr.inner, rhs.inner)
            } else {
              lhs = Syntax::Get(lhs, expr.inner)
            }
          }
          PairedOpen(Parenthesis) =>
            if self.peek_token!().inner.tag == PairedClose(Parenthesis) {
              self.skip_token()
              ntokens += 1
              lhs = Syntax::App(lhs, targs=None, [])
            } else {
              let expr = self.parse_expr!()
              ntokens += expr.ntokens
              let exprs = [expr.inner]
              while self.peek_operator(",") {
                self.skip_token()
                ntokens += 1
                let expr = self.parse_expr!()
                ntokens += expr.ntokens
                exprs.push(expr.inner)
              }
              let _ = self.expect_paired_close!(Parenthesis)
              ntokens += 1
              match (lhs, exprs) {
                (Syntax::Var("not"), [expr]) => lhs = Syntax::Not(expr)
                (Syntax::Var("<Array::make>"), [e1, e2]) =>
                  lhs = Syntax::Array(e1, e2)
                (Syntax::CtorApp(f, []), es) => lhs = Syntax::CtorApp(f, es)
                (Syntax::CtorApp(_, _), _) =>
                  fail!("CtorApp has zero arguments before check apply style")
                _ => lhs = Syntax::App(lhs, targs=None, exprs)
              }
            }
          _ => fail!("unexpected token when parsing expr")
        }
        continue
      }
      None => ()
    }
    match self.infix(op_str) {
      Some((laff, raff)) => {
        if laff < level {
          break
        }
        self.skip_token()
        ntokens += 1
        let rhs = self.parse_expr_with_level!(raff)
        ntokens += rhs.ntokens
        match op_str {
          "<=" => lhs = Syntax::LE(lhs, rhs.inner)
          "==" => lhs = Syntax::Eq(lhs, rhs.inner)
          "+" => lhs = Syntax::Prim(lhs, rhs.inner, Op::Add, kind=None)
          "-" => lhs = Syntax::Prim(lhs, rhs.inner, Op::Sub, kind=None)
          "*" => lhs = Syntax::Prim(lhs, rhs.inner, Op::Mul, kind=None)
          "/" => lhs = Syntax::Prim(lhs, rhs.inner, Op::Div, kind=None)
          _ =>
            match self.op.get(op_str) {
              Some({ func, .. }) =>
                lhs = Syntax::App(
                  Syntax::Var(func),
                  targs=None,
                  [lhs, rhs.inner],
                )
              None => fail!("unexpected operator in infix expression")
            }
        }
        continue
      }
      None => ()
    }
    break
  }
  { inner: lhs, ntokens }
}

fn Parser::bind_types(
  names : Array[String],
  types : Type?
) -> Array[(String, Type)]!Failure {
  match types {
    Some(Type::Tuple(tys)) => {
      if names.length() != tys.length() {
        fail!("number of names and types do not match")
      }
      let x = []
      for i = 0; i < names.length(); i = i + 1 {
        x.push((names[i], tys[i]))
      }
      x
    }
    Some(_) => fail!("expected tuple type")
    None => names.map(fn(name) { (name, Type::new_var()) })
  }
}

fn Parser::bind_type(name : String, ty : Type?) -> (String, Type) {
  (name, ty.or(Type::new_var()))
}

pub fn Parser::parse_stmt(self : Parser, top : Bool) -> PSyntax!Failure {
  let lookahead = self.peek_token!()
  let mut ntokens = 0
  let expr = match lookahead.inner.tag {
    Kw(Keyword::Enum) => {
      self.skip_token()
      ntokens += 1
      if not(top) || lookahead.inner.loc.column != 1 {
        fail!(
          "enum definition must be at the top level and the beginning of the line, but got enum at \{lookahead.inner.loc}",
        )
      }
      let ident = self.expect_ident!()
      ntokens += ident.ntokens
      let _ = self.expect_paired_open!(Paired::Brace)
      ntokens += 1
      // collect ctors
      let ctors = []
      while self.peek_token!().inner.tag != PairedClose(Brace) {
        let ctor = self.expect_ident!()
        ntokens += ctor.ntokens
        let lookahead3 = self.peek_token!()
        // parse constructor arguments
        let tys = if lookahead3.inner.tag == PairedOpen(Parenthesis) {
          self.skip_token()
          ntokens += 1
          // require at least one argument
          let ty = self.parse_type!()
          ntokens += ty.ntokens
          let tys = [ty.inner]
          if self.peek_operator(",") {
            while self.peek_operator(",") {
              self.skip_token()
              ntokens += 1
              let ty = self.parse_type!()
              ntokens += ty.ntokens
              tys.push(ty.inner)
            }
            let _ = self.expect_paired_close!(Parenthesis)
            ntokens += 1
          } else {
            let _ = self.expect_paired_close!(Parenthesis)
            ntokens += 1
          }
          tys
        } else {
          []
        }
        let _ = self.expect_operator!(";")
        ntokens += 1
        ctors.push((ctor.inner.span, tys))
      }
      let _ = self.expect_paired_close!(Paired::Brace)
      ntokens += 1
      let enum_ty = Type::Enum(ident.inner.span, Ref::new(Some(ctors)))
      if self.peek_operator(";") {
        self.skip_token()
        ntokens += 1
        if self.reach_eof() == false {
          let stmt = self.parse_stmt!(top)
          ntokens += stmt.ntokens
          Syntax::LetEnum(ident.inner.span, enum_ty, stmt.inner)
        } else {
          Syntax::LetEnum(ident.inner.span, enum_ty, Syntax::Unit)
        }
      } else {
        Syntax::LetEnum(ident.inner.span, enum_ty, Syntax::Unit)
      }
    }
    Kw(Keyword::Infix) => {
      self.skip_token()
      ntokens += 1
      let assoc_left = if self.peek_keyword(Left) {
        true
      } else if self.peek_keyword(Right) {
        false
      } else {
        fail!("expected left or right after infix")
      }
      self.skip_token()
      ntokens += 1
      let prec = self.expect_int!()
      ntokens += prec.ntokens
      let prec = prec.inner.to_int()
      let op = self.next_token!()
      ntokens += op.ntokens
      match op {
        { inner: { tag: Op(op), .. }, .. } => {
          let ident = self.expect_ident!()
          ntokens += ident.ntokens
          let func = ident.inner.span
          self.op.set(op, { assoc_left, prec, func })
          let _ = self.expect_operator!(";")
          ntokens += 1
          return self.parse_stmt!(top)
        }
        _ => fail!("expected operator after infix left/right")
      }
    }
    Kw(Keyword::Let) => {
      self.skip_token()
      ntokens += 1
      if top && lookahead.inner.loc.column != 1 {
        fail!(
          "let definition must be at the beginning of the line, but got let at \{lookahead.inner.loc}",
        )
      }
      let lookahead2 = self.peek_token!()
      match lookahead2.inner.tag {
        PairedOpen(Parenthesis) => {
          // let (...) : ... = ...
          self.skip_token()
          ntokens += 1
          let ident = self.expect_ident!()
          ntokens += ident.ntokens
          let idents = [ident.inner.span]
          while self.peek_operator(",") {
            self.skip_token()
            ntokens += 1
            let ident = self.expect_ident!()
            ntokens += ident.ntokens
            idents.push(ident.inner.span)
          }
          let _ = self.expect_paired_close!(Parenthesis)
          ntokens += 1
          let ty = if self.peek_operator(":") {
            self.skip_token()
            ntokens += 1
            let ty = self.parse_type!()
            ntokens += ty.ntokens
            Some(ty.inner)
          } else {
            None
          }
          let _ = self.expect_operator!("=")
          ntokens += 1
          let expr = self.parse_expr!()
          ntokens += expr.ntokens
          if self.peek_operator(";") {
            self.skip_token()
            ntokens += 1
            if self.reach_eof() == false {
              let stmt = self.parse_stmt!(top)
              ntokens += stmt.ntokens
              Syntax::LetTuple(
                Parser::bind_types!(idents, ty),
                expr.inner,
                stmt.inner,
              )
            } else {
              Syntax::LetTuple(
                Parser::bind_types!(idents, ty),
                expr.inner,
                Syntax::Unit,
              )
            }
          } else {
            Syntax::LetTuple(
              Parser::bind_types!(idents, ty),
              expr.inner,
              Syntax::Unit,
            )
          }
        }
        Identifer => {
          // let ... : ... = ...
          self.skip_token()
          ntokens += 1
          let ident = lookahead2
          let ty = if self.peek_operator(":") {
            self.skip_token()
            ntokens += 1
            let ty = self.parse_type!()
            ntokens += ty.ntokens
            Some(ty.inner)
          } else {
            None
          }
          let _ = self.expect_operator!("=")
          ntokens += 1
          let expr = self.parse_expr!()
          ntokens += expr.ntokens
          if self.peek_operator(";") {
            self.skip_token()
            ntokens += 1
            if self.reach_eof() == false {
              let stmt = self.parse_stmt!(top)
              ntokens += stmt.ntokens
              Syntax::Let(
                Parser::bind_type(ident.inner.span, ty),
                expr.inner,
                stmt.inner,
              )
            } else {
              Syntax::Let(
                Parser::bind_type(ident.inner.span, ty),
                expr.inner,
                Syntax::Unit,
              )
            }
          } else {
            Syntax::Let(
              Parser::bind_type(ident.inner.span, ty),
              expr.inner,
              Syntax::Unit,
            )
          }
        }
        _ => fail!("unexpected token after let")
      }
    }
    Kw(Keyword::Class) => {
      self.skip_token()
      ntokens += 1
      let ident = self.expect_ident!()
      ntokens += ident.ntokens
      let name = ident.inner.span
      if name[0] < 'A' || name[0] > 'Z' {
        fail!("class name must start with uppercase letter")
      }
      let tyvar = []
      let _ = self.expect_paired_open!(Paired::Bracket)
      ntokens += 1
      let type_var = self.expect_ident!()
      ntokens += type_var.ntokens
      tyvar.push(type_var.inner.span)
      // Currently we only support must one and only one type variable
      let _ = self.expect_paired_close!(Paired::Bracket)
      ntokens += 1
      let _ = self.expect_paired_open!(Paired::Brace)
      ntokens += 1
      let methods = []
      while self.peek_token!().inner.tag != PairedClose(Brace) {
        let method_name = self.expect_ident!()
        ntokens += method_name.ntokens
        let _ = self.expect_operator!(":")
        ntokens += 1
        let ty = self.parse_type!()
        ntokens += ty.ntokens
        let _ = self.expect_operator!(";")
        ntokens += 1
        methods.push((method_name.inner.span, ty.inner))
      }
      let _ = self.expect_paired_close!(Paired::Brace)
      ntokens += 1
      let tyvar = tyvar[0]
      if self.peek_operator(";") {
        self.skip_token()
        ntokens += 1
        if self.reach_eof() == false {
          let stmt = self.parse_stmt!(top)
          ntokens += stmt.ntokens
          Syntax::LetClass({ name, tyvar, methods }, stmt.inner)
        } else {
          Syntax::LetClass({ name, tyvar, methods }, Syntax::Unit)
        }
      } else {
        Syntax::LetClass({ name, tyvar, methods }, Syntax::Unit)
      }
    }
    Kw(Keyword::Instance) => {
      self.skip_token()
      ntokens += 1
      let ident = self.expect_ident!()
      ntokens += ident.ntokens
      let name = ident.inner.span
      if name[0] < 'A' || name[0] > 'Z' {
        fail!("class name must start with uppercase letter")
      }
      let tys = []
      let _ = self.expect_paired_open!(Paired::Bracket)
      ntokens += 1
      let ty = self.parse_type!()
      ntokens += ty.ntokens
      tys.push(ty)
      // Currently we only support must one and only one type variable
      let _ = self.expect_paired_close!(Paired::Bracket)
      ntokens += 1
      let _ = self.expect_paired_open!(Paired::Brace)
      ntokens += 1
      let methods = []
      while self.peek_token!().inner.tag != PairedClose(Brace) {
        let method_name = self.expect_ident!()
        ntokens += method_name.ntokens
        let _ = self.expect_operator!("=")
        ntokens += 1
        let expr = self.parse_expr!()
        ntokens += ty.ntokens
        let _ = self.expect_operator!(";")
        ntokens += 1
        methods.push((method_name.inner.span, expr.inner))
      }
      let _ = self.expect_paired_close!(Paired::Brace)
      ntokens += 1
      let ty = tys[0].inner
      if self.peek_operator(";") {
        self.skip_token()
        ntokens += 1
        if self.reach_eof() == false {
          let stmt = self.parse_stmt!(top)
          ntokens += stmt.ntokens
          Syntax::LetInstance({ name, ty, methods }, stmt.inner)
        } else {
          Syntax::LetInstance({ name, ty, methods }, Syntax::Unit)
        }
      } else {
        Syntax::LetInstance({ name, ty, methods }, Syntax::Unit)
      }
    }
    Kw(Keyword::Fn) => {
      self.skip_token()
      ntokens += 1
      if top && lookahead.inner.loc.column != 1 {
        fail!(
          "fn definition must be at the beginning of the line, but got let at \{lookahead.inner.loc}",
        )
      }
      let ident = self.expect_ident!()
      ntokens += ident.ntokens
      let name = ident.inner.span
      let args = []
      fn parse_non_generic() -> Unit!Failure {
        if top && (name == "main" || name == "init") {
          fail!("main and init cannot have arguments when they are top-level")
        }
        let _ = self.expect_paired_open!(Paired::Parenthesis)
        ntokens += 1
        if self.peek_token!().inner.tag != PairedClose(Parenthesis) {
          let arg = self.expect_ident!()
          ntokens += arg.ntokens
          if self.peek_operator(":") {
            self.skip_token()
            ntokens += 1
            let ty = self.parse_type!()
            ntokens += ty.ntokens
            args.push(Parser::bind_type(arg.inner.span, Some(ty.inner)))
          } else {
            args.push(Parser::bind_type(arg.inner.span, None))
          }
          while self.peek_operator(",") {
            self.skip_token()
            ntokens += 1
            let arg = self.expect_ident!()
            ntokens += arg.ntokens
            if self.peek_operator(":") {
              self.skip_token()
              ntokens += 1
              let ty = self.parse_type!()
              ntokens += ty.ntokens
              args.push(Parser::bind_type(arg.inner.span, Some(ty.inner)))
            } else {
              args.push(Parser::bind_type(arg.inner.span, None))
            }
          }
        }
        let _ = self.expect_paired_close!(Paired::Parenthesis)
        ntokens += 1
      }

      let tyvars = []
      if self.peek_token!().inner.tag == PairedOpen(Parenthesis) {
        parse_non_generic!()
      } else if self.peek_token!().inner.tag == PairedOpen(Bracket) {
        self.skip_token()
        ntokens += 1
        let type_var = self.expect_ident!()
        ntokens += 1
        let cstr = if self.peek_operator(":") {
          self.skip_token()
          ntokens += 1
          let cstr = self.expect_ident!()
          ntokens += cstr.ntokens
          Some(cstr.inner.span)
        } else {
          None
        }
        tyvars.push((type_var.inner.span, cstr))
        while self.peek_operator(",") {
          self.skip_token()
          ntokens += 1
          let type_var = self.expect_ident!()
          ntokens += 1
          let cstr = if self.peek_operator(":") {
            self.skip_token()
            ntokens += 1
            let cstr = self.expect_ident!()
            ntokens += cstr.ntokens
            Some(cstr.inner.span)
          } else {
            None
          }
          tyvars.push((type_var.inner.span, cstr))
        }
        let _ = self.expect_paired_close!(Bracket)
        ntokens += 1
        parse_non_generic!()
      }
      let ty = if self.peek_operator("->") {
        self.skip_token()
        ntokens += 1
        let ty = self.parse_type!()
        ntokens += ty.ntokens
        Some(ty.inner)
      } else {
        None
      }
      let _ = self.expect_paired_open!(Paired::Brace)
      ntokens += 1
      let stmt = self.parse_stmt!(false)
      ntokens += stmt.ntokens
      let _ = self.expect_paired_close!(Paired::Brace)
      ntokens += 1
      let fdef = @types.Fundef::{
        tyvars,
        name: Parser::bind_type(name, ty),
        args,
        body: stmt.inner,
      }
      if self.peek_operator(";") {
        self.skip_token()
        ntokens += 1
        if self.reach_eof() {
          Syntax::LetRec(fdef, Syntax::Unit)
        } else {
          let stmt = self.parse_stmt!(top)
          ntokens += stmt.ntokens
          Syntax::LetRec(fdef, stmt.inner)
        }
      } else {
        Syntax::LetRec(fdef, Syntax::Unit)
      }
    }
    Eof | PairedClose(_) => Syntax::Unit
    _ => {
      let expr = self.parse_expr!()
      ntokens += expr.ntokens
      if self.peek_operator(";") {
        self.skip_token()
        ntokens += 1
        let stmt = self.parse_stmt!(top)
        ntokens += stmt.ntokens
        Syntax::Let(Parser::bind_type("_", None), expr.inner, stmt.inner)
      } else {
        expr.inner
      }
    }
  }
  { inner: expr, ntokens }
}

pub fn Parser::parse(self : Parser) -> PSyntax!Failure {
  let stmt = self.parse_stmt!(true)
  if self.reach_eof() == false {
    fail!("unexpected token after parsing: \{self.peek_token!().inner}")
  }
  stmt
}
