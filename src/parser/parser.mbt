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

typealias MutMap[K, V] = @hashmap.T[K, V]

pub struct Parser {
  tokenizer : Lexer
  tracer : @util.SubTracer
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

fn Parser::prefix(op : String) -> (Int, Int)? {
  match op {
    "-" => Some((Parser::level_none(), Parser::prefix_start() + 1))
    _ => None
  }
}

fn Parser::postfix(op : String) -> (Int, Int)? {
  match op {
    "(" => Some((Parser::postfix_start() + 1, Parser::level_none()))
    "[" => Some((Parser::postfix_start() + 1, Parser::level_none()))
    _ => None
  }
}

fn Parser::infix(op : String) -> (Int, Int)? {
  match op {
    "<=" => Some((Parser::infix_start() + 1, Parser::infix_start() + 2))
    "==" => Some((Parser::infix_start() + 1, Parser::infix_start() + 2))
    "+" => Some((Parser::infix_start() + 3, Parser::infix_start() + 4))
    "-" => Some((Parser::infix_start() + 3, Parser::infix_start() + 4))
    "*" => Some((Parser::infix_start() + 5, Parser::infix_start() + 6))
    "/" => Some((Parser::infix_start() + 5, Parser::infix_start() + 6))
    _ => None
  }
}

fn Parser::type_postfix(op : String) -> (Int, Int)? {
  match op {
    "(" => Some((Parser::postfix_start() + 1, Parser::level_none()))
    "[" => Some((Parser::postfix_start() + 1, Parser::level_none()))
    _ => None
  }
}

fn Parser::type_infix(op : String) -> (Int, Int)? {
  match op {
    "->" => Some((Parser::infix_start() + 2, Parser::infix_start() + 1))
    _ => None
  }
}

pub fn Parser::new(input : String, tracer : @util.SubTracer) -> Parser {
  { tokenizer: Lexer::new(input), tracer, token: None }
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
    match Parser::type_postfix(op_str) {
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
    match Parser::type_infix(op_str) {
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
        _ => Syntax::Var(lhs_token.inner.span)
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
          let lookhead = self.peek_token!()
          if lookhead.inner.tag != PairedOpen(Brace) {
            fail!("expected matched block")
          }
          self.skip_token()
          ntokens += 1
          // parse match arms
          fn syntax_to_pattern(node : Syntax) -> Pattern!Failure {
            match node {
              Syntax::Var("_") => Pattern::Wildcard
              Syntax::Unit => Pattern::Unit
              Syntax::Bool(b) => Pattern::Bool(b)
              Syntax::Int(n) => Pattern::Int(n)
              Syntax::Double(n) => Pattern::Double(n)
              Syntax::Var(name) => Pattern::Var(name)
              Syntax::Tuple(exprs) => {
                let ts = []
                for expr in exprs {
                  ts.push(syntax_to_pattern!(expr))
                }
                Pattern::Tuple(ts)
              }
              Syntax::CtorApp(Syntax::Var(ctor), exprs) => {
                let args = []
                for expr in exprs {
                  args.push(syntax_to_pattern!(expr))
                }
                Pattern::CtorApp(ctor, args)
              }
              _ => fail!("unexpected syntax in pattern")
            }
          }

          let mut lookhead2 = self.peek_token!()
          let match_arms = []
          while lookhead2.inner.tag != PairedClose(Brace) {
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
            lookhead2 = self.peek_token!()
          }
          self.skip_token()
          ntokens += 1
          Syntax::Match(scrutinee.inner, match_arms)
        }
        _ => fail!("keyword not supported in expression: \{kw}")
      }
    Op(op) =>
      match Parser::prefix(op) {
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
    match Parser::postfix(op_str) {
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
              lhs = Syntax::App(lhs, [])
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
                (Syntax::Var(f), es) =>
                  if f[0] >= 'A' && f[0] <= 'Z' {
                    lhs = Syntax::CtorApp(lhs, es)
                  } else {
                    lhs = Syntax::App(lhs, exprs)
                  }
                _ => lhs = Syntax::App(lhs, exprs)
              }
            }
          _ => fail!("unexpected token when parsing expr")
        }
        continue
      }
      None => ()
    }
    match Parser::infix(op_str) {
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
          _ => fail!("unexpected operator in infix expression")
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
      let ident = self.next_token!()
      ntokens += ident.ntokens
      if ident.inner.tag != Identifer {
        fail!("expected enum identifier")
      }
      let _ = self.expect_paired_open!(Paired::Brace)
      ntokens += 1
      // collect ctors
      let ctors = []
      while self.peek_token!().inner.tag != PairedClose(Brace) {
        let ctor = self.next_token!()
        ntokens += ctor.ntokens
        if ctor.inner.tag != Identifer {
          fail!("expected constructor identifier")
        }
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
      let _ = self.next_token!()
      ntokens += 1
      let enum_ty = Type::Enum(ident.inner.span, ctors)
      if self.reach_eof() == false {
        let stmt = self.parse_stmt!(top)
        ntokens += stmt.ntokens
        Syntax::LetEnum(enum_ty, stmt.inner)
      } else {
        Syntax::LetEnum(enum_ty, Syntax::Unit)
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
          let ident = self.next_token!()
          ntokens += ident.ntokens
          if ident.inner.tag != Identifer {
            fail!("expected identifier")
          }
          let idents = [ident.inner.span]
          while self.peek_operator(",") {
            self.skip_token()
            ntokens += 1
            let ident = self.next_token!()
            ntokens += ident.ntokens
            if ident.inner.tag != Identifer {
              fail!("expected identifier")
            }
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
    Kw(Keyword::Fn) => {
      self.skip_token()
      ntokens += 1
      if top && lookahead.inner.loc.column != 1 {
        fail!(
          "fn definition must be at the beginning of the line, but got let at \{lookahead.inner.loc}",
        )
      }
      let ident = self.next_token!()
      ntokens += ident.ntokens
      if ident.inner.tag != Identifer {
        fail!("expected identifier")
      }
      let name = ident.inner.span
      let args = []
      fn parse_non_generic() -> Unit!Failure {
        if top && (name == "main" || name == "init") {
          fail!("main and init cannot have arguments when they are top-level")
        }
        let _ = self.expect_paired_open!(Paired::Parenthesis)
        ntokens += 1
        if self.peek_token!().inner.tag != PairedClose(Parenthesis) {
          let arg = self.next_token!()
          ntokens += arg.ntokens
          if arg.inner.tag != Identifer {
            fail!("expected identifier")
          }
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
            let arg = self.next_token!()
            ntokens += arg.ntokens
            if arg.inner.tag != Identifer {
              fail!("expected identifier")
            }
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
        let type_var = self.next_token!()
        ntokens += 1
        if ident.inner.tag != Identifer {
          fail!("expected identifier")
        }
        tyvars.push(type_var.inner.span)
        while self.peek_operator(",") {
          self.skip_token()
          ntokens += 1
          let type_var = self.next_token!()
          ntokens += 1
          if ident.inner.tag != Identifer {
            fail!("expected identifier")
          }
          tyvars.push(type_var.inner.span)
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
