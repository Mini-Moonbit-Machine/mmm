pub typealias MutMap[K, V] = @hashmap.T[K, V]

pub typealias MutSet[K] = @hashset.T[K]

pub struct Occurrence {
  order : Array[Int]
} derive(Eq)

pub fn Occurrence::to_string(self : Occurrence) -> String {
  Show::to_string(self)
}

pub fn Occurrence::output(self : Occurrence, logger : Logger) -> Unit {
  logger.write_string("/\\")
  for i in self.order {
    logger.write_string(".\{i}")
  }
}

pub impl Hash for Occurrence with hash_combine(self, hasher) {
  for i in self.order {
    hasher.combine_int(i)
  }
}

pub struct Occurrences {
  occurs : Array[Occurrence]
} derive(Show)

/// empty, written as 'Λ' occurence.
pub fn Occurrence::empty() -> Occurrence {
  Occurrence::{ order: [] }
}

pub fn Occurrence::destruct(self : Occurrence, suborder : Int) -> Occurrence {
  let order = self.order.copy()
  order.push(suborder)
  Occurrence::{ order, }
}

pub fn Occurrences::root() -> Occurrences {
  Occurrences::{ occurs: [Occurrence::empty()] }
}

pub fn Occurrences::unpack_col(
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

pub fn Occurrences::remove_col(self : Occurrences, nth : Int) -> Occurrences {
  let new_occurs = self.occurs.copy()
  let _ = new_occurs.remove(nth)
  Occurrences::{ occurs: new_occurs }
}

pub enum SubPatSpec {
  Unit
  BoolLit(~bval : Bool)
  IntLit(~ival : Int64)
  DoubleLit(~dval : Double)
  Ctor(~name : String, ~csize : Int)
  Tuple(~tsize : Int)
} derive(Eq)

pub fn SubPatSpec::to_string(self : SubPatSpec) -> String {
  Show::to_string(self)
}

pub fn SubPatSpec::output(self : SubPatSpec, logger : Logger) -> Unit {
  match self {
    Unit => logger.write_string("unit")
    BoolLit(~bval) => logger.write_string("bool(\{bval})")
    IntLit(~ival) => logger.write_string("int(\{ival})")
    DoubleLit(~dval) => logger.write_string("double(\{dval})")
    Ctor(~name, ~csize) => logger.write_string("ctor(\{name}, \{csize})")
    Tuple(~tsize) => logger.write_string("tuple(\{tsize})")
  }
}

impl Hash for SubPatSpec with hash_combine(self, hasher) {
  match self {
    Unit => hasher.combine_int(0)
    BoolLit(~bval) => hasher.combine_bool(bval)
    IntLit(~ival) => hasher.combine_int64(ival)
    DoubleLit(~dval) => hasher.combine_double(dval)
    Ctor(~name, ~csize) => {
      hasher.combine_string(name)
      hasher.combine_int(csize)
    }
    Tuple(~tsize) => hasher.combine_int(tsize)
  }
}

fn SubPatSpec::from_pat(pat : @types.Pattern) -> SubPatSpec? {
  match pat {
    Unit => Some(Unit)
    Bool(bval) => Some(BoolLit(~bval))
    Int(ival) => Some(IntLit(~ival))
    Double(dval) => Some(DoubleLit(~dval))
    CtorApp(name, pats) => Some(Ctor(~name, csize=pats.length()))
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
    (Ctor(~name, ~csize), CtorApp(n, ps)) => name == n && csize == ps.length()
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
    Ctor(~csize, ..) => csize
    Tuple(~tsize) => tsize
  }
}

pub typealias BindCtx = @immut/hashmap.T[Occurrence, String]

pub enum DecisionTree {
  Succeed(~binds : BindCtx, ~action : @types.Syntax)
  Fail
  Switch(
    ~occur : Occurrence,
    ~cases : Array[(SubPatSpec, DecisionTree)],
    ~default : DecisionTree?
  )
}

struct PatMatrixRow {
  binds : BindCtx
  patterns : Array[@types.Pattern]
  action : @types.Syntax
} derive(Show)

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
} derive(Show)

fn PatMatrix::new(match_arms : Array[@types.MatchArm]) -> PatMatrix {
  // root clause matrixs
  let arity = 1
  let occurs = Occurrences::root()
  let rows = match_arms.map(
    fn(arm) {
      PatMatrixRow::{
        binds: BindCtx::new(),
        patterns: [arm.pat],
        action: arm.action,
      }
    },
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
  self.tracer.println("matcher_run: init mat\n\{mat}")
  let tree = self.clause_matrixs_to_decison_tree(mat)
  self.tracer.println("matcher_run: final tree\n\{tree.to_pretty_print()}")
  tree
}

fn PatternMatcher::specialize_row(
  self : PatternMatcher,
  ctor : SubPatSpec,
  occ : Occurrence,
  selected_col : Int,
  row : PatMatrixRow
) -> PatMatrixRow? {
  let subpat_arity = ctor.get_subpattern_arity()
  // try unpack nth col ctor
  let selected_subpat = row.patterns[selected_col]
  let unpack_subpat = match selected_subpat {
    Wildcard => Some(([@types.Wildcard].repeat(subpat_arity), None))
    Int(_) | Bool(_) | Double(_) | Unit =>
      if ctor.is_compatible_with(selected_subpat) {
        if subpat_arity != 0 {
          self.tracer.abort("Int, Bool, Double, Unit should have zero arity")
        }
        Some(([], None))
      } else {
        None
      }
    Var(x) =>
      Some(
        ([@types.Wildcard].repeat(subpat_arity), Some(row.binds.add(occ, x))),
      )
    Tuple(pats) =>
      if ctor.is_compatible_with(selected_subpat) {
        Some((pats, None))
      } else {
        None
      }
    CtorApp(_, pats) =>
      if ctor.is_compatible_with(selected_subpat) {
        Some((pats, None))
      } else {
        None
      }
  }
  let new_pat_vec = []
  match unpack_subpat {
    None => None
    Some((subpat, bind)) => {
      for i, pat in row.patterns {
        if i == selected_col {
          new_pat_vec.append(subpat)
        } else {
          new_pat_vec.push(pat)
        }
      }
      let new_bind = match bind {
        None => row.binds
        Some(bind) => bind
      }
      Some(
        PatMatrixRow::{
          binds: new_bind,
          patterns: new_pat_vec,
          action: row.action,
        },
      )
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
    let occ = mat.occurs.occurs[selected_col]
    match self.specialize_row(ctor, occ, selected_col, row) {
      None => continue
      Some(new_row) => new_rows.push(new_row)
    }
  }
  PatMatrix::{ arity: new_arity, occurs: new_occurs, rows: new_rows }
}

fn PatternMatcher::pattern_identifier(pat : @types.Pattern) -> String? {
  match pat {
    Var(name) => Some(name)
    _ => None
  }
}

fn PatternMatcher::default_row(
  occ : Occurrence,
  selected_col : Int,
  row : PatMatrixRow
) -> PatMatrixRow? {
  // try to discard unmatched selected column
  let selected_subpat = row.patterns[selected_col]
  let discard_subpat = match selected_subpat {
    Wildcard => Some(None)
    Int(_) | Bool(_) | Double(_) | Unit => None // literals unmatched always be discarded
    Var(_) => Some(None)
    Tuple(_) => None
    CtorApp(_, _) => None
  }
  match discard_subpat {
    None => None
    Some(bind) => {
      let new_pat_vec = row.patterns.copy()
      let _ = new_pat_vec.remove(selected_col)
      let new_bind = match bind {
        None => row.binds
        Some(name) => row.binds.add(occ, name)
      }
      Some(
        PatMatrixRow::{
          binds: new_bind,
          patterns: new_pat_vec,
          action: row.action,
        },
      )
    }
  }
}

/// default matrix on selected column
fn PatternMatcher::default_matrix(
  selected_col : Int,
  mat : PatMatrix
) -> PatMatrix {
  let new_occurs = mat.occurs.remove_col(selected_col)
  let new_arity = mat.arity - 1
  let new_rows = []
  for row in mat.rows {
    let occ = mat.occurs.occurs[selected_col]
    // filter rows and match wildcard pattern
    match default_row(occ, selected_col, row) {
      None => continue
      Some(new_row) => new_rows.push(new_row)
    }
  }
  PatMatrix::{ arity: new_arity, occurs: new_occurs, rows: new_rows }
}

fn PatternMatcher::select_by_column_ctor_prefix(
  mat : PatMatrix,
  col : Int
) -> Int {
  let mut score = 0
  for row in mat.rows {
    let subpat = row.patterns[col]
    match subpat {
      CtorApp(_, _) => score += 1
      _ => break
    }
  }
  score
}

fn PatternMatcher::select_by_small_branching_factor(
  mat : PatMatrix,
  col : Int
) -> Int {
  let ctors = MutSet::new()
  for row in mat.rows {
    let subpat = SubPatSpec::from_pat(row.patterns[col])
    match subpat {
      Some(pat) => ctors.insert(pat)
      _ => ()
    }
  }
  -ctors.size()
}

fn PatternMatcher::select_by_small_arity(mat : PatMatrix, col : Int) -> Int {
  let mut score = 0
  for row in mat.rows {
    let subpat = SubPatSpec::from_pat(row.patterns[col])
    match subpat {
      Some(Ctor(~csize, ..)) => score += csize
      Some(Tuple(~tsize)) => score += tsize
      _ => ()
    }
  }
  -score
}

fn PatternMatcher::select_col(mat : PatMatrix) -> Int {
  let heuristic = [
    fn(mat, col) { select_by_column_ctor_prefix(mat, col) },
    fn(mat, col) { select_by_small_branching_factor(mat, col) },
    fn(mat, col) { select_by_small_arity(mat, col) },
  ]
  let mut columns = Array::makei(mat.arity, fn(i) { i })
  let mut choose_column = columns[0]
  for f in heuristic {
    let scores = columns.map(fn(col) { f(mat, col) })
    let mut best_score = scores[0]
    for score in scores {
      if score > best_score {
        best_score = score
      }
    }
    // filter columns with best score
    let filter_colunns = []
    for i, scores in scores {
      if scores == best_score {
        filter_colunns.push(columns[i])
      }
    }
    columns = filter_colunns
    if columns.is_empty() {
      break
    }
    choose_column = columns[0]
  }
  choose_column
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
    let mut binds = mat.rows[0].binds
    for i, subpat in mat.rows[0].patterns {
      let occ = mat.occurs.occurs[i]
      match pattern_identifier(subpat) {
        Some(name) => binds = binds.add(occ, name)
        None => ()
      }
    }
    return DecisionTree::Succeed(~binds, action=mat.rows[0].action)
  }
  // Select a column and then switching
  let pattern_actions = []
  let selected_col = select_col(mat)
  let specialize_cache = MutMap::new()
  for row in mat.rows {
    let selected_pattern = row.patterns[selected_col]
    guard let Some(subpatspec) = SubPatSpec::from_pat(selected_pattern) else {
      _ => continue
    }
    let submatrix = match specialize_cache.get(subpatspec) {
      Some(_) => continue // same ctor, no need to specialize
      None => {
        let submatrix = self.specialize_matrix(subpatspec, selected_col, mat)
        specialize_cache.set(subpatspec, submatrix)
        submatrix
      }
    }
    self.tracer.println(
      "specialize_matrix: by subpat \{subpatspec}, at col \{selected_col}:\n\{submatrix}",
    )
    let subtree = self.clause_matrixs_to_decison_tree(submatrix)
    pattern_actions.push((subpatspec, subtree))
  }
  let default_submatrix = default_matrix(selected_col, mat)
  self.tracer.println("default_matrix:\n\{default_submatrix}")
  let default_tree = if not(default_submatrix.rows.is_empty()) {
    Some(self.clause_matrixs_to_decison_tree(default_submatrix))
  } else {
    None
  }
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

pub type PrettyDecisionTree DecisionTree

pub fn DecisionTree::to_pretty_print(self : DecisionTree) -> PrettyDecisionTree {
  self
}

pub fn PrettyDecisionTree::print_one_level(
  self : PrettyDecisionTree,
  logger : @util.IndentLogger,
  tree : DecisionTree
) -> Unit {
  match tree {
    Succeed(~binds, ~action) => {
      logger.write_string("Succeed({")
      for bind in binds {
        let (occur, name) = bind
        logger.write_string("\{occur} -> \{name}, ")
      }
      logger.write_string("}, \{action})")
    }
    Fail => logger.write_string("Fail")
    Switch(~occur, ~cases, ~default) => {
      logger.write_string("Switch(\{occur}, \n")
      logger.indent()
      logger.write_string("[")
      for subp in cases {
        let (subpat, subtree) = subp
        logger.write_string("(\{subpat}, ")
        self.print_one_level(logger, subtree)
        logger.write_string("),\n")
      }
      match default {
        None => logger.write_string("\n")
        Some(default) => {
          logger.write_string("(fallback, ")
          self.print_one_level(logger, default)
          logger.write_string(")\n")
        }
      }
      logger.outdent()
      logger.write_string("])")
    }
  }
}

pub fn PrettyDecisionTree::to_string(self : PrettyDecisionTree) -> String {
  Show::to_string(self)
}

impl Show for PrettyDecisionTree with output(
  self : PrettyDecisionTree,
  logger : Logger
) -> Unit {
  let ident_logger = @util.IndentLogger::new(logger)
  self.print_one_level(ident_logger, self._)
}
