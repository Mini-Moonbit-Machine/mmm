typealias Parser = @parser.Parser

fn test_parse_type(
  source : String,
  expected_str : String,
  expected_ntokens : Int
) -> Unit!Error {
  let parser = Parser::new(source)
  let ty = parser.parse_type!()
  let ty_ntokens = ty.ntokens
  assert_eq!(ty.inner.to_json().stringify(), expected_str)
  assert_eq!(ty_ntokens, expected_ntokens)
  let last = parser.next_token!()
  assert_eq!(last.inner.tag, @lex.TokenTag::Eof)
}

test "parse type" {
  test_parse_type!(
    "Unit",
    #|"Unit"
    ,
    1,
  )
  test_parse_type!(
    "Int",
    #|"Int"
    ,
    1,
  )
  test_parse_type!(
    "Bool",
    #|"Bool"
    ,
    1,
  )
  test_parse_type!(
    "Double",
    #|"Float"
    ,
    1,
  )
  test_parse_type!(
    "Array[Unit]",
    #|["Array","Unit"]
    ,
    4,
  )
  test_parse_type!(
    "Array[Array[Double]]",
    #|["Array",["Array","Float"]]
    ,
    7,
  )
  test_parse_type!(
    "(Int, Double, Unit)",
    #|["Tuple",["Int","Float","Unit"]]
    ,
    7,
  )
  test_parse_type!(
    "(Int, (Double, Unit)) -> (Int, Double)",
    #|["Fun",["Int",["Tuple",["Float","Unit"]]],["Tuple",["Int","Float"]]]
    ,
    15,
  )
}
