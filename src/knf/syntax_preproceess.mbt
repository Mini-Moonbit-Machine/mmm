/// Preprocesses the syntax tree, extracting the contents of functions `main` and `init` to top-level.
pub fn KnfEnv::syntax_preprocess(
  self : KnfEnv,
  s : @types.Syntax,
  has_main : Bool,
  has_init : Bool
) -> @types.Syntax {
  match s {
    Unit =>
      if has_init && has_main {
        Let(("_", Unit), App(Var("init"), []), App(Var("main"), []))
      } else if has_main {
        App(Var("main"), [])
      } else if has_init {
        App(Var("init"), [])
      } else {
        Unit
      }
    LetTuple(xs, e1, e2) =>
      LetTuple(xs, e1, self.syntax_preprocess(e2, has_main, has_init))
    LetRec(f, e) => {
      let mut has_init = has_init
      let mut has_main = has_main
      if f.name.0 == "init" {
        has_init = true
      } else if f.name.0 == "main" {
        has_main = true
      }
      LetRec(f, self.syntax_preprocess(e, has_main, has_init))
    }
    Let(ann, e1, e2) =>
      Let(ann, e1, self.syntax_preprocess(e2, has_main, has_init))
    _ => s
  }
}
