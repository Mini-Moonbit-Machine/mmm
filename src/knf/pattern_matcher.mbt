pub typealias MutMap[K, V] = @hashmap.T[K, V]

pub struct Occurrence {
  order : Array[Int]
} derive(Show)

pub struct Occurrences {
  occurs : Array[Occurrence]
} derive(Show)

/// empty, written as 'Λ' occurence.
fn Occurrence::empty() -> Occurrence {
  Occurrence::{ order: [] }
}

fn Occurrences::root() -> Occurrences {
  Occurrences::{ occurs: [Occurrence::empty()] }
}

fn Occurrences::unpack_col(
  self : Occurrences,
  nth : Int,
  arity : Int
) -> Occurrences {
  // destruct nth column
  let new_occurs = self.occurs.copy()
  let nth_occur = new_occurs.remove(nth)
  for i in 0..<arity {
    let destruct_nth_occur = nth_occur.order.copy()
    destruct_nth_occur.push(i)
    new_occurs.insert(nth + i, Occurrence::{ order: destruct_nth_occur })
  }
  Occurrences::{ occurs: new_occurs }
}

fn Occurrences::remove_col(self : Occurrences, nth : Int) -> Occurrences {
  let new_occurs = self.occurs.copy()
  let _ = new_occurs.remove(nth)
  Occurrences::{ occurs: new_occurs }
}

pub enum SubPatSpec {
  Unit
  BoolLit(~bval : Bool)
  IntLit(~ival : Int64)
  DoubleLit(~dval : Double)
  Tuple(~tsize : Int)
} derive(Show)

fn SubPatSpec::from_pat(pat : @types.Pattern) -> SubPatSpec? {
  match pat {
    Unit => Some(Unit)
    Bool(bval) => Some(BoolLit(~bval))
    Int(ival) => Some(IntLit(~ival))
    Double(dval) => Some(DoubleLit(~dval))
    Tuple(ts) => Some(Tuple(tsize=ts.length()))
    Wildcard | Var(_) => None
  }
}

fn SubPatSpec::is_compatible_with(
  self : SubPatSpec,
  pat : @types.Pattern
) -> Bool {
  match (self, pat) {
    (Unit, Unit) => true
    (BoolLit(~bval), Bool(b)) => bval == b
    (IntLit(~ival), Int(i)) => ival == i
    (DoubleLit(~dval), Double(d)) => dval == d
    (Tuple(~tsize), Tuple(ts)) => tsize == ts.length()
    _ => false
  }
}

fn SubPatSpec::get_subpattern_arity(self : SubPatSpec) -> Int {
  match self {
    Unit => 0
    BoolLit(_) => 0
    IntLit(_) => 0
    DoubleLit(_) => 0
    Tuple(~tsize) => tsize
  }
}

pub enum DecisionTree {
  Succeed(~action : @types.Syntax)
  Fail
  Switch(
    ~occur : Occurrence,
    ~cases : Array[(SubPatSpec, DecisionTree)],
    ~default : DecisionTree
  )
} derive(Show)

struct PatMatrixRow {
  patterns : Array[@types.Pattern]
  action : @types.Syntax
}

fn PatMatrixRow::all_wildcards(self : PatMatrixRow) -> Bool {
  self.patterns
  .iter()
  .all(
    fn(pat) {
      match pat {
        Wildcard => true
        Var(_) => true
        _ => false
      }
    },
  )
}

/// Matrixs of clause matrix P -> A
pub struct PatMatrix {
  arity : Int
  occurs : Occurrences
  rows : Array[PatMatrixRow]
}

fn PatMatrix::new(match_arms : Array[@types.MatchArm]) -> PatMatrix {
  // root clause matrixs
  let arity = 1
  let occurs = Occurrences::root()
  let rows = match_arms.map(
    fn(arm) { PatMatrixRow::{ patterns: [arm.pat], action: arm.action } },
  )
  PatMatrix::{ arity, occurs, rows }
}

fn PatMatrix::is_empty(self : PatMatrix) -> Bool {
  self.rows.is_empty()
}

pub struct PatternMatcher {
  expr : @types.Syntax
  tracer : @util.SubTracer
}

pub fn PatternMatcher::new(
  expr : @types.Syntax,
  tracer : @util.SubTracer
) -> PatternMatcher {
  PatternMatcher::{ expr, tracer }
}

pub fn PatternMatcher::run(self : PatternMatcher) -> DecisionTree {
  let mat = self.build_clause_matrixs(self.expr)
  self.clause_matrixs_to_decison_tree(mat)
}

fn PatternMatcher::specialize_row(
  self : PatternMatcher,
  ctor : SubPatSpec,
  selected_col : Int,
  row : PatMatrixRow
) -> PatMatrixRow? {
  let subpat_arity = ctor.get_subpattern_arity()
  // try unpack nth col ctor
  let selected_subpat = row.patterns[selected_col]
  let unpack_subpat = match selected_subpat {
    Wildcard => Some([@types.Wildcard].repeat(subpat_arity))
    Int(_) | Bool(_) | Double(_) | Unit => {
      if ctor.is_compatible_with(selected_subpat) && subpat_arity != 0 {
        self.tracer.abort("Int, Bool, Double, Unit should have zero arity")
      }
      Some([])
    }
    Var(_) => Some([@types.Wildcard].repeat(subpat_arity))
    Tuple(pats) =>
      if ctor.is_compatible_with(selected_subpat) {
        Some(pats)
      } else {
        None
      }
    CtorApp(_, _) => self.tracer.abort("CtorApp is not supported currently")
  }
  let new_pat_vec = []
  match unpack_subpat {
    None => None
    Some(subpat) => {
      for i, pat in row.patterns {
        if i == selected_col {
          new_pat_vec.append(subpat)
        } else {
          new_pat_vec.push(pat)
        }
      }
      Some(PatMatrixRow::{ patterns: new_pat_vec, action: row.action })
    }
  }
}

/// specialization operation by constructor on selected column
fn PatternMatcher::specialize_matrix(
  self : PatternMatcher,
  ctor : SubPatSpec,
  selected_col : Int,
  mat : PatMatrix
) -> PatMatrix {
  let subpat_arity = ctor.get_subpattern_arity()
  let new_occurs = mat.occurs.unpack_col(selected_col, subpat_arity)
  let new_arity = mat.arity + subpat_arity - 1
  let new_rows = []
  for row in mat.rows {
    // filter rows and unpack constructor occurence & patterns
    match self.specialize_row(ctor, selected_col, row) {
      None => continue
      Some(new_row) => new_rows.push(new_row)
    }
  }
  PatMatrix::{ arity: new_arity, occurs: new_occurs, rows: new_rows }
}

fn PatternMatcher::default_row(
  self : PatternMatcher,
  selected_col : Int,
  row : PatMatrixRow
) -> PatMatrixRow? {
  // try to discard unmatched selected column
  let selected_subpat = row.patterns[selected_col]
  let discard_subpat = match selected_subpat {
    Wildcard => Some(())
    Int(_) | Bool(_) | Double(_) | Unit =>
      self.tracer.abort("Int, Bool, Double, Unit should have zero arity")
    Var(_) => Some(())
    Tuple(pats) => None
    CtorApp(_, _) => self.tracer.abort("CtorApp is not supported currently")
  }
  match discard_subpat {
    None => None
    Some(_) => {
      let new_pat_vec = row.patterns.copy()
      let _ = new_pat_vec.remove(selected_col)
      Some(PatMatrixRow::{ patterns: new_pat_vec, action: row.action })
    }
  }
}

/// default matrix on selected column
fn PatternMatcher::default_matrix(
  self : PatternMatcher,
  selected_col : Int,
  mat : PatMatrix
) -> PatMatrix {
  let new_occurs = mat.occurs.remove_col(selected_col)
  let new_arity = mat.arity - 1
  let new_rows = []
  for row in mat.rows {
    // filter rows and match wildcard pattern
    match self.default_row(selected_col, row) {
      None => continue
      Some(new_row) => new_rows.push(new_row)
    }
  }
  PatMatrix::{ arity: new_arity, occurs: new_occurs, rows: new_rows }
}

fn PatternMatcher::selected_col(self : PatternMatcher, mat : PatMatrix) -> Int {
  // fallback strategy: select the first column
  0
}

fn PatternMatcher::clause_matrixs_to_decison_tree(
  self : PatternMatcher,
  mat : PatMatrix
) -> DecisionTree {
  // If matrix P has no rows, always fail
  if mat.is_empty() {
    return DecisionTree::Fail
  }
  // If the first row of P exists and is constituted by wildcards, then matching always succeeds and yields the first action.
  if mat.rows[0].all_wildcards() {
    return DecisionTree::Succeed(action=mat.rows[0].action)
  }
  // Select a column and then switching
  let pattern_actions = []
  let selected_col = self.selected_col(mat)
  for row in mat.rows {
    let selected_pattern = row.patterns[selected_col]
    guard let Some(subpatsepc) = SubPatSpec::from_pat(selected_pattern) else {
      _ => continue
    }
    let submatrix = self.specialize_matrix(subpatsepc, selected_col, mat)
    let subtree = self.clause_matrixs_to_decison_tree(submatrix)
    pattern_actions.push((subpatsepc, subtree))
  }
  let default_submatrix = self.default_matrix(selected_col, mat)
  let default_tree = self.clause_matrixs_to_decison_tree(default_submatrix)
  let switch_occur = mat.occurs.occurs[selected_col]
  DecisionTree::Switch(
    occur=switch_occur,
    cases=pattern_actions,
    default=default_tree,
  )
}

fn PatternMatcher::build_clause_matrixs(
  self : PatternMatcher,
  node : @types.Syntax
) -> PatMatrix {
  guard let Match(_, arms) = node else {
    _ => self.tracer.abort("not a match expression")
  }
  PatMatrix::new(arms)
}
