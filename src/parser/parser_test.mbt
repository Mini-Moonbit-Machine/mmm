typealias Parser = @parser.Parser

fn test_parse_type(
  source : String,
  expected_str : String,
  expected_ntokens : Int
) -> Unit!Error {
  let tracer = @util.Tracer::new()
  let subtracer = tracer.new_subtracer("Test")
  let parser = Parser::new(source, subtracer)
  let ty = parser.parse_type!()
  let ty_ntokens = ty.ntokens
  assert_eq!(ty.inner.to_json().stringify(), expected_str)
  assert_eq!(ty_ntokens, expected_ntokens)
  let last = parser.next_token!()
  assert_eq!(last.inner.tag, @lexer.TokenTag::Eof)
}

fn test_parse_expr(
  source : String,
  expected_str : String,
  expected_ntokens : Int
) -> Unit!Error {
  let tracer = @util.Tracer::new()
  let subtracer = tracer.new_subtracer("Test")
  let parser = Parser::new(source, subtracer)
  let expr = parser.parse_expr!()
  let expr_ntokens = expr.ntokens
  assert_eq!(expr.inner.to_json().stringify(), expected_str)
  assert_eq!(expr_ntokens, expected_ntokens)
  let last = parser.next_token!()
  assert_eq!(last.inner.tag, @lexer.TokenTag::Eof)
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

test "parse expr" {
  test_parse_expr!(
    "1",
    #|["Int",1]
    ,
    1,
  )
  test_parse_expr!(
    "10.000000001",
    #|["Float",10.000000001]
    ,
    1,
  )
  test_parse_expr!(
    "1 + 2",
    #|["IAdd",["Int",1],["Int",2]]
    ,
    3,
  )
  test_parse_expr!(
    "1 + 2 * 3",
    #|["IAdd",["Int",1],["IMul",["Int",2],["Int",3]]]
    ,
    5,
  )
  test_parse_expr!(
    "1 * 2 + 3",
    #|["IAdd",["IMul",["Int",1],["Int",2]],["Int",3]]
    ,
    5,
  )
  test_parse_expr!(
    "1 * (2 + 3)",
    #|["IMul",["Int",1],["IAdd",["Int",2],["Int",3]]]
    ,
    7,
  )
  test_parse_expr!(
    "1 * (2 + 3) * 4",
    #|["IMul",["IMul",["Int",1],["IAdd",["Int",2],["Int",3]]],["Int",4]]
    ,
    9,
  )
  test_parse_expr!(
    "()",
    #|"Unit"
    ,
    2,
  )
  test_parse_expr!(
    "true",
    #|["Bool",true]
    ,
    1,
  )
  test_parse_expr!(
    "false",
    #|["Bool",false]
    ,
    1,
  )
  test_parse_expr!(
    "(1)",
    #|["Int",1]
    ,
    3,
  )
  test_parse_expr!(
    "(1, 2)",
    #|["Tuple",[["Int",1],["Int",2]]]
    ,
    5,
  )
  test_parse_expr!(
    "(1, 2, 3)",
    #|["Tuple",[["Int",1],["Int",2],["Int",3]]]
    ,
    7,
  )
  test_parse_expr!(
    "Array::make(1, 2)",
    #|["Array",["Int",1],["Int",2]]
    ,
    8,
  )
  test_parse_expr!(
    "not(true)",
    #|["Not",["Bool",true]]
    ,
    4,
  )
  test_parse_expr!(
    "1 == 2",
    #|["Eq",["Int",1],["Int",2]]
    ,
    3,
  )
  test_parse_expr!(
    "1 <= 2",
    #|["LE",["Int",1],["Int",2]]
    ,
    3,
  )
  test_parse_expr!(
    "2 + 1 * 3 / 2 - 1 <= 5 + 2 * f(x, y) + arr[2] + y()",
    #|["LE",["ISub",["IAdd",["Int",2],["IDiv",["IMul",["Int",1],["Int",3]],["Int",2]]],["Int",1]],["IAdd",["IAdd",["IAdd",["Int",5],["IMul",["Int",2],["App",["Var","f"],[["Var","x"],["Var","y"]]]]],["Get",["Var","arr"],["Int",2]]],["App",["Var","y"],[]]]]
    ,
    29,
  )
  test_parse_expr!(
    "if true { 1 }",
    #|["If",["Bool",true],["Int",1],"Unit"]
    ,
    5,
  )
  test_parse_expr!(
    "if true { 1 } else { 2 }",
    #|["If",["Bool",true],["Int",1],["Int",2]]
    ,
    9,
  )
  test_parse_expr!(
    "(sin(cos(sqrt(abs_float(-12.3)))) + 4.5 - 6.7 * 8.9 / 1.23456789) * float_of_int(1000000)",
    #|["IMul",["ISub",["IAdd",["App",["Var","sin"],[["App",["Var","cos"],[["App",["Var","sqrt"],[["App",["Var","abs_float"],[["Neg",["Float",12.3]]]]]]]]]],["Float",4.5]],["IDiv",["IMul",["Float",6.7],["Float",8.9]],["Float",1.23456789]]],["App",["Var","float_of_int"],[["Int",1000000]]]]
    ,
    29,
  )
  test_parse_expr!(
    #|c[i] = x
    ,
    #|["Put",["Var","c"],["Var","i"],["Var","x"]]
    ,
    6,
  )
}

test "parse stmt" {
  test_parse_expr!(
    #|{
    #|2
    #|}
    ,
    #|["Int",2]
    ,
    3,
  )
  test_parse_expr!(
    #|{
    #|  let x : Int = 2;
    #|  3
    #|}
    ,
    #|["Let",["x","Int"],["Int",2],["Int",3]]
    ,
    10,
  )
  test_parse_expr!(
    #|{
    #|  let (x, y): (Int, Double) = (1, 2);
    #|  3
    #|}
    ,
    #|["LetTuple",[["x","Int"],["y","Float"]],["Tuple",[["Int",1],["Int",2]]],["Int",3]]
    ,
    22,
  )
  test_parse_expr!(
    #|{
    #|  fn f(x: Int) -> Int {
    #|    x
    #|  };
    #|  3
    #|}
    ,
    #|["LetRec",{"name":["f","Int"],"args":[["x","Int"]],"body":["Var","x"]},["Int",3]]
    ,
    16,
  )
  test_parse_expr!(
    #| { c[i] = x; 1 }
    ,
    #|["Let",["_",["Var","None"]],["Put",["Var","c"],["Var","i"],["Var","x"]],["Int",1]]
    ,
    10,
  )
}

test "debug" {

}
