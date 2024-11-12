typealias Syntax = @types.Syntax

typealias MatchArm = @types.MatchArm

typealias Pattern = @types.Pattern

test "paper_list_example_tuple_impl" {
  let tracer = @util.Tracer::new()
  let subtracer = tracer.new_subtracer("Test", true, false)
  let match_expr = Syntax::Match(
    Tuple([Var("xs"), Var("ys")]),
    [
      // Nil, _
      MatchArm::{ pat: Tuple([Tuple([]), Wildcard]), action: Int(0) },
      // _, Nil
      MatchArm::{ pat: Tuple([Wildcard, Tuple([])]), action: Int(1) },
      // Cons(_, _), Cons(_, _)
      MatchArm::{
        pat: Tuple(
          [Tuple([Var("x"), Var("xs'")]), Tuple([Var("y"), Var("ys")])],
        ),
        action: Int(2),
      },
    ],
  )
  let matcher = @knf.PatternMatcher::new(match_expr, subtracer)
  let tree = matcher.run()
  println(tree)
}
