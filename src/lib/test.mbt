test "interp" {
  assert_eq!(
    Interpreter::new().eval!(
      If(Op(Eq, Int(1), Int(2)), Bool(true), Bool(false)),
    ),
    Value::bool(false),
  )
  let fib = fn(n) {
    Expr::LetRec(
      [
        {
          name: "fib",
          params: ["n"],
          body: If(
            Op(Eq, Var("n"), Int(0)),
            Int(0),
            If(
              Op(Eq, Var("n"), Int(1)),
              Int(1),
              Op(
                Add,
                App(Var("fib"), [Op(Sub, Var("n"), Int(1))]),
                App(Var("fib"), [Op(Sub, Var("n"), Int(2))]),
              ),
            ),
          ),
        },
      ],
      Expr::App(Var("fib"), [Int(n)]),
    )
  }
  let anf = fn(n) { to_anf(fib(n)) }
  let renamed = fn(n) { IrBuilder::new().alpha_rename(anf(n)) }
  assert_eq!(Interpreter::new().eval!(anf(10)), Value::int(55))
  assert_eq!(Interpreter::new().eval!(renamed(20)), Value::int(6765))
}
