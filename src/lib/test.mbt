test "interp" {
  assert_eq!(
    Interpreter::new().eval!(
      If(Op(Eq, Int(1), Int(2)), Bool(true), Bool(false)),
    ),
    Value::bool(false),
  )
  let fib = fn(n) {
    Node::LetRec(
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
      Node::App(Var("fib"), [Int(n)]),
    )
  }
  assert_eq!(Interpreter::new().eval!(fib(10)), Value::int(55))
}
