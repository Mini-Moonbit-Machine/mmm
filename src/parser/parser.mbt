typealias Lexer = @lex.Lexer

typealias Token = @lex.Token

typealias TokenTag = @lex.TokenTag

typealias Paired = @lex.Paired

typealias Keyword = @lex.Keyword

typealias Type = @types.Type

typealias Syntax = @types.Syntax

pub struct Parser {
  tokenizer : Lexer
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

fn Parser::prefix(op: String) -> (Int, Int)? {
  match op {
    "+" => Some((Parser::level_none(), Parser::prefix_start() + 1))
    "-" => Some((Parser::level_none(), Parser::prefix_start() + 1))
    _ => None
  }
}

fn Parser::postfix(op: String) -> (Int, Int)? {
  match op {
    "(" => Some((Parser::postfix_start() + 1, Parser::level_none()))
    "[" => Some((Parser::postfix_start() + 1, Parser::level_none()))
    _ => None
  }
}

fn Parser::infix(op: String) -> (Int, Int)? {
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

fn Parser::type_postfix(op: String) -> (Int, Int)? {
  match op {
    "(" => Some((Parser::postfix_start() + 1, Parser::level_none()))
    "[" => Some((Parser::postfix_start() + 1, Parser::level_none()))
    _ => None
  }
}

fn Parser::type_infix(op: String) -> (Int, Int)? {
  match op {
    "->" => Some((Parser::infix_start() + 2, Parser::infix_start() + 1))
    _ => None
  }
}

pub fn Parser::new(input : String) -> Parser {
  { tokenizer: Lexer::new(input), token: None }
}

fn Parser::skip_token(self : Parser) -> Unit {
  self.token = None
}

fn Parser::peek_token(self : Parser) -> PToken!Failure {
  let token = match self.token {
    Some(tok) => { inner: tok, ntokens: 1 }
    None => {
      let tok = self.tokenizer.next_err!()
      self.token = Some(tok)
      { inner: tok, ntokens: 1 }
    }
  }
  if token.inner.tag == TokenTag::Newline {
    self.peek_token!()
  } else {
    token
  }
}

pub fn Parser::next_token(self : Parser) -> PToken!Failure {
  match self.token {
    Some(tok) => {
      self.token = None
      { inner: tok, ntokens: 1 }
    }
    None => { inner: self.tokenizer.next_err!(), ntokens: 1 }
  }
}

fn Parser::expect_reach_eof(self : Parser) -> Unit!Failure {
  match self.peek_token!() {
    { inner: { tag: TokenTag::Eof, .. }, .. } => ()
    _ => fail!("expected eof")
  }
}

fn Parser::expect_paired_open(self : Parser, po : Paired) -> PToken!Failure {
  match self.next_token!() {
    { inner: { tag: TokenTag::PairedOpen(po2), .. }, .. } as r =>
      if po == po2 {
        r
      } else {
        fail!("expected paired open")
      }
    _ => fail!("expected paired open")
  }
}

fn Parser::expect_paired_close(self : Parser, pc : Paired) -> PToken!Failure {
  match self.next_token!() {
    { inner: { tag: TokenTag::PairedClose(pc2), .. }, .. } as r =>
      if pc == pc2 {
        r
      } else {
        fail!("expected paired close")
      }
    _ => fail!("expected paired close")
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
    fail!("expected keyword")
  }
}

fn Parser::expect_operator(self : Parser, op : String) -> PToken!Failure {
  if self.peek_operator(op) {
    self.next_token!()
  } else {
    fail!("expected operator")
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
        _ => fail!("unexpected type identifier")
      }
    PairedOpen(po) => {
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
    _ => fail!("unexpected token when parsing type")
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
  
  { inner: lhs, ntokens: ntokens }
}
