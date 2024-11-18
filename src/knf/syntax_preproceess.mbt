/// Preprocesses the syntax tree, extracting the contents of functions `main` and `init` to top-level.
pub fn KnfEnv::syntax_preprocess(
  self : KnfEnv,
  s : @types.Syntax,
  has_main : Bool
) -> @types.Syntax {
  match s {
    Unit => if has_main { App(Var("main"), []) } else { Unit }
    LetTuple(xs, e1, e2) =>
      LetTuple(xs, e1, self.syntax_preprocess(e2, has_main))
    LetRec(f, e) =>
      if f.name.0 == "init" {
        LetRec(
          f,
          Let(
            ("_", Unit),
            App(Var("init"), []),
            self.syntax_preprocess(e, has_main),
          ),
        )
      } else {
        LetRec(f, self.syntax_preprocess(e, has_main || f.name.0 == "main"))
      }
    Let(ann, e1, e2) => Let(ann, e1, self.syntax_preprocess(e2, has_main))
    LetEnum(enumty, e) => LetEnum(enumty, self.syntax_preprocess(e, has_main))
    _ => s
  }
}
