typealias FreeSet = @hashset.T[Name]

pub fn knf_fdef_free_vars(f : FuncDef) -> FreeSet {
  let fv = knf_free_vars(f.body)
  f.args.each(fn(argt) { fv.remove(argt.0) })
  fv.remove(f.name)
  fv
}

pub fn knf_free_vars(k : Knf) -> FreeSet {
  match k {
    Unit | Bool(_) | Int(_) | Double(_) => FreeSet::new()
    Neg(x) | FNeg(x) => FreeSet::of([x])
    Add(x, y)
    | Sub(x, y)
    | Mul(x, y) | Div(x, y) | FAdd(x, y) | FSub(x, y) | FMul(x, y) | FDiv(x, y) =>
      FreeSet::of([x, y])
    IfEq(x, y, k1, k2) | IfLe(x, y, k1, k2) => {
      let fv1 = knf_free_vars(k1)
      let fv2 = knf_free_vars(k2)
      let fv = fv1.union(fv2)
      fv.insert(x)
      fv.insert(y)
      fv
    }
    Var(x) => FreeSet::of([x])
    Apply(f, args) => {
      let fv = FreeSet::from_array(args)
      fv.insert(f)
      fv
    }
    Tuple(fields) => FreeSet::from_array(fields)
    UnboxedTuple(fields) => FreeSet::from_array(fields)
    Let((x, _), e1, e2) => {
      let fv1 = knf_free_vars(e1)
      let fv2 = knf_free_vars(e2)
      // x is bounded
      fv2.remove(x)
      let fv = fv1.union(fv2)
      fv
    }
    LetRec(f, k) => {
      let fv1 = knf_fdef_free_vars(f)
      let fv2 = knf_free_vars(k)
      fv2.remove(f.name)
      let fv = fv1.union(fv2)
      fv
    }
    LetTuple(xts, y, k) => {
      let fv = knf_free_vars(k)
      // xts is bounded
      xts.each(fn(xt) { fv.remove(xt.0) })
      fv.insert(y)
      fv
    }
    LetUnboxedTuple(xts, y, k) => {
      let fv = knf_free_vars(k)
      // xts is bounded
      xts.each(fn(xt) { fv.remove(xt.0) })
      fv.insert(y)
      fv
    }
    Get(array, index) => FreeSet::of([array, index])
    Put(array, index, value) => FreeSet::of([array, index, value])
    ExternalArray(_) => FreeSet::new()
    ExternalFunctionApplication(_, args) => FreeSet::from_array(args)
  }
}

pub struct FreeSetClosure {
  ctx : Map[Name, FreeSet]
  mut changed : Bool
}

pub fn FreeSetClosure::new() -> FreeSetClosure {
  { ctx: Map::new(), changed: false }
}

pub fn compute(
  self : FreeSetClosure,
  knf : Knf
) -> (FreeSet, Map[Name, FreeSet]) {
  self.changed = true
  self.ctx.clear()
  let mut fv = FreeSet::new()
  while self.changed {
    self.changed = false
    fv = self.knf_free_vars_closure(knf)
  }
  (fv, self.ctx)
}

// didn't handle same name well
pub fn knf_fdef_free_vars_closure(
  self : FreeSetClosure,
  f : FuncDef
) -> FreeSet {
  let fvs = self.knf_free_vars_closure(f.body)
  f.args.each(fn(argt) { fvs.remove(argt.0) })
  fvs.remove(f.name)
  let new_fvs = []
  fvs.each(
    fn(fv) {
      match self.ctx.get(fv) {
        Some(fv2) => new_fvs.push_iter(fv2.iter())
        _ => ()
      }
    },
  )
  new_fvs.each(fn(fv) { fvs.insert(fv) })
  match self.ctx.get(f.name) {
    Some(fv2) => {
      let fv3 = fvs.union(fv2)
      self.changed = self.changed || fv3.size() != fv2.size()
      self.ctx.set(f.name, fv3)
    }
    None => {
      if fvs.size() != 0 {
        self.changed = true
      }
      self.ctx.set(f.name, fvs)
    }
  }
  fvs
}

pub fn knf_free_vars_closure(self : FreeSetClosure, k : Knf) -> FreeSet {
  match k {
    Unit | Bool(_) | Int(_) | Double(_) => FreeSet::new()
    Neg(x) | FNeg(x) => FreeSet::of([x])
    Add(x, y)
    | Sub(x, y)
    | Mul(x, y) | Div(x, y) | FAdd(x, y) | FSub(x, y) | FMul(x, y) | FDiv(x, y) =>
      FreeSet::of([x, y])
    IfEq(x, y, k1, k2) | IfLe(x, y, k1, k2) => {
      let fv1 = self.knf_free_vars_closure(k1)
      let fv2 = self.knf_free_vars_closure(k2)
      let fv = fv1.union(fv2)
      fv.insert(x)
      fv.insert(y)
      fv
    }
    Var(x) => FreeSet::of([x])
    Apply(f, args) =>
      match self.ctx.get(f) {
        Some(fv2) => {
          let fv = FreeSet::from_array(args)
          fv.insert(f)
          fv2.union(fv)
        }
        None => {
          let fv = FreeSet::from_array(args)
          fv.insert(f)
          fv
        }
      }
    Tuple(fields) => FreeSet::from_array(fields)
    UnboxedTuple(fields) => FreeSet::from_array(fields)
    Let((x, _), e1, e2) => {
      let fv1 = self.knf_free_vars_closure(e1)
      let fv2 = self.knf_free_vars_closure(e2)
      // x is bounded
      fv2.remove(x)
      let fv = fv1.union(fv2)
      fv
    }
    LetRec(f, k) => {
      let fv1 = self.knf_fdef_free_vars_closure(f)
      let fv2 = self.knf_free_vars_closure(k)
      fv2.remove(f.name)
      let fv = fv1.union(fv2)
      fv
    }
    LetTuple(xts, y, k) => {
      let fv = self.knf_free_vars_closure(k)
      // xts is bounded
      xts.each(fn(xt) { fv.remove(xt.0) })
      fv.insert(y)
      fv
    }
    LetUnboxedTuple(xts, y, k) => {
      let fv = self.knf_free_vars_closure(k)
      // xts is bounded
      xts.each(fn(xt) { fv.remove(xt.0) })
      fv.insert(y)
      fv
    }
    Get(array, index) => FreeSet::of([array, index])
    Put(array, index, value) => FreeSet::of([array, index, value])
    ExternalArray(_) => FreeSet::new()
    ExternalFunctionApplication(_, args) => FreeSet::from_array(args)
  }
}
