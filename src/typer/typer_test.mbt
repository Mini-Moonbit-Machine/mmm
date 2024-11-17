fn test_tychk(source : String) -> Unit!Error {
  let tracer = @util.Tracer::new()
  let subtracer = tracer.new_subtracer("Test", true, false)
  let parser = @parser.Parser::new(source, subtracer)
  let ast = parser.parse!()
  let typer = @typer.Typer::new(ast.inner, subtracer)
  typer.check_all!() |> ignore
}

test "tychk1" {
  test_tychk!(
    #|fn dummy(i : Int) -> Int {
    #|  i
    #|};
    #|let factorial : (Int) -> Int = {
    #|  fn g(f) {
    #|    fn h(x) {
    #|      if x == 0 {
    #|        1
    #|      } else {
    #|        x * f(x - 1)
    #|      }
    #|    };
    #|    h
    #|  };
    #|  dummy
    #|};
    ,
  )
}

test "tychk2" {
  test_tychk!(
    #|fn dummy(i : Int) -> Int {
    #|  i
    #|};
    #|let factorial : (Int) -> Int = {
    #|  fn g(f: Int -> Int) {
    #|    fn h(x) {
    #|      if x == 0 {
    #|        1
    #|      } else {
    #|        x * f(x - 1)
    #|      }
    #|    };
    #|    h
    #|  };
    #|  dummy
    #|};
    ,
  )
}
