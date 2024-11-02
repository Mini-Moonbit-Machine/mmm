pub struct CallAnalysis {
  core : @core.Core
  tracer : @util.SubTracer
}

pub type FnCallFreq Map[@core.FnRef, CallFrequnce] derive(Show)

pub fn CallAnalysis::new(
  core : @core.Core,
  tracer : @util.SubTracer
) -> CallAnalysis {
  { core, tracer }
}

pub enum CallFrequnce {
  Never
  Once
  Multiple
  MaybeInClosure
} derive(Show)

fn add_call(fcf : FnCallFreq, f : @core.FnRef) -> Unit {
  let freq = fcf._.get(f).or(Never)
  let next = match freq {
    Never => Once
    Once => Multiple
    Multiple => Multiple
    MaybeInClosure => MaybeInClosure
  }
  fcf._.set(f, next)
}

fn add_closure_call(fcf : FnCallFreq, f : @core.FnRef) -> Unit {
  fcf._.set(f, MaybeInClosure)
}

fn compute_value(fcf : FnCallFreq, value : @core.Value) -> Unit {
  match value {
    Fn(~fref) => add_closure_call(fcf, fref)
    Self(~fref) => add_closure_call(fcf, fref)
    _ => ()
  }
}

fn compute_fill(fcf : FnCallFreq, fill : @core.Fill) -> Unit {
  match fill {
    Init(~val) => compute_value(fcf, val)
    Field(~val, ..) => compute_value(fcf, val)
    _ => ()
  }
}

fn compute_insn(fcf : FnCallFreq, insn : @core.Insn) -> Unit {
  match insn {
    Call(~f, ..) => add_call(fcf, f)
    Apply(~f, ..) => compute_value(fcf, f)
    Store(~val, ..) => compute_value(fcf, val)
    Alloc(~fills, ..) => fills.each(fn(fill) { compute_fill(fcf, fill) })
    _ => ()
  }
}

fn compute_fn(fcf : FnCallFreq, func : @core.Fn) -> Unit {
  for block in func.blocks {
    let (_, block) = block
    if not(block.is_basic_block()) {
      continue
    }
    let bb = block.bb_or_header_bb(func)
    for insn in bb.seq {
      let insn = func.insns[insn].unwrap()
      compute_insn(fcf, insn)
    }
  }
}

pub fn CallAnalysis::compute(self : CallAnalysis) -> FnCallFreq {
  let fcf = Map::new()
  for func in self.core.funcs {
    let (fref, _) = func
    fcf.set(fref, Never)
  }
  for func in self.core.funcs {
    let (_, func) = func
    compute_fn(fcf, func)
  }
  for symbol in self.core.global_symbols {
    let (_, symbol) = symbol
    add_call(fcf, symbol.thunk)
  }
  fcf
}
