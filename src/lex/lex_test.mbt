struct TestToken {
  tag : TokenTag
  span : String
}

typealias TokenTag = @lex.TokenTag

typealias Keyword = @lex.Keyword

typealias Paired = @lex.Paired

fn test_tokenize(input : String, expected : Array[TestToken]) -> Unit!Error {
  let tokenizer = @lex.Lexer::new(input)
  for etok in expected {
    let tok = tokenizer.next_err!()
    assert_eq!(tok.tag, etok.tag)
    assert_eq!(tok.span, etok.span)
  }
  let last_token = tokenizer.next_err!()
  assert_eq!(last_token.tag, TokenTag::Eof)
}

test "Keywords" {
  test_tokenize!("fn", [{ tag: TokenTag::Kw(Keyword::Fn), span: "fn" }])
  test_tokenize!("let", [{ tag: TokenTag::Kw(Keyword::Let), span: "let" }])
  test_tokenize!("if", [{ tag: TokenTag::Kw(Keyword::If), span: "if" }])
  test_tokenize!("else", [{ tag: TokenTag::Kw(Keyword::Else), span: "else" }])
}

test "Operators" {
  test_tokenize!(".", [{ tag: TokenTag::Op("."), span: "." }])
  test_tokenize!("+", [{ tag: TokenTag::Op("+"), span: "+" }])
  test_tokenize!("-", [{ tag: TokenTag::Op("-"), span: "-" }])
  test_tokenize!("*", [{ tag: TokenTag::Op("*"), span: "*" }])
  test_tokenize!("/", [{ tag: TokenTag::Op("/"), span: "/" }])
  test_tokenize!("=", [{ tag: TokenTag::Op("="), span: "=" }])
  test_tokenize!("==", [{ tag: TokenTag::Op("=="), span: "==" }])
  test_tokenize!("<=", [{ tag: TokenTag::Op("<="), span: "<=" }])
  test_tokenize!("->", [{ tag: TokenTag::Op("->"), span: "->" }])
  test_tokenize!(":", [{ tag: TokenTag::Op(":"), span: ":" }])
  test_tokenize!(";", [{ tag: TokenTag::Op(";"), span: ";" }])
  test_tokenize!(",", [{ tag: TokenTag::Op(","), span: "," }])
}

test "Paired" {
  test_tokenize!(
    "(",
    [{ tag: TokenTag::PairedOpen(Paired::Parenthesis), span: "(" }],
  )
  test_tokenize!(
    ")",
    [{ tag: TokenTag::PairedClose(Paired::Parenthesis), span: ")" }],
  )
  test_tokenize!(
    "[",
    [{ tag: TokenTag::PairedOpen(Paired::Bracket), span: "[" }],
  )
  test_tokenize!(
    "]",
    [{ tag: TokenTag::PairedClose(Paired::Bracket), span: "]" }],
  )
  test_tokenize!("{", [{ tag: TokenTag::PairedOpen(Paired::Brace), span: "{" }])
  test_tokenize!(
    "}",
    [{ tag: TokenTag::PairedClose(Paired::Brace), span: "}" }],
  )
}

test "Example Ack" {
  let content =
    #|fn ack(x : Int, y : Int) -> Int {
    #|if x <= 0 {
    #|  y + 1
    #|} else {
    #|  if y <= 0 {
    #|    ack
  test_tokenize!(
    content,
    [
      { tag: TokenTag::Kw(Keyword::Fn), span: "fn" },
      { tag: TokenTag::Identifer, span: "ack" },
      { tag: TokenTag::PairedOpen(Paired::Parenthesis), span: "(" },
      { tag: TokenTag::Identifer, span: "x" },
      { tag: TokenTag::Op(":"), span: ":" },
      { tag: TokenTag::Identifer, span: "Int" },
      { tag: TokenTag::Op(","), span: "," },
      { tag: TokenTag::Identifer, span: "y" },
      { tag: TokenTag::Op(":"), span: ":" },
      { tag: TokenTag::Identifer, span: "Int" },
      { tag: TokenTag::PairedClose(Paired::Parenthesis), span: ")" },
      { tag: TokenTag::Op("->"), span: "->" },
      { tag: TokenTag::Identifer, span: "Int" },
      { tag: TokenTag::PairedOpen(Paired::Brace), span: "{" },
      { tag: TokenTag::Newline, span: "\n" },
      { tag: TokenTag::Kw(Keyword::If), span: "if" },
      { tag: TokenTag::Identifer, span: "x" },
      { tag: TokenTag::Op("<="), span: "<=" },
      { tag: TokenTag::IntLiteral(0), span: "0" },
      { tag: TokenTag::PairedOpen(Paired::Brace), span: "{" },
      { tag: TokenTag::Newline, span: "\n" },
      { tag: TokenTag::Identifer, span: "y" },
      { tag: TokenTag::Op("+"), span: "+" },
      { tag: TokenTag::IntLiteral(1), span: "1" },
      { tag: TokenTag::Newline, span: "\n" },
      { tag: TokenTag::PairedClose(Paired::Brace), span: "}" },
      { tag: TokenTag::Kw(Keyword::Else), span: "else" },
      { tag: TokenTag::PairedOpen(Paired::Brace), span: "{" },
      { tag: TokenTag::Newline, span: "\n" },
      { tag: TokenTag::Kw(Keyword::If), span: "if" },
      { tag: TokenTag::Identifer, span: "y" },
      { tag: TokenTag::Op("<="), span: "<=" },
      { tag: TokenTag::IntLiteral(0), span: "0" },
      { tag: TokenTag::PairedOpen(Paired::Brace), span: "{" },
      { tag: TokenTag::Newline, span: "\n" },
      { tag: TokenTag::Identifer, span: "ack" },
    ],
  )
}
